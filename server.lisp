#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.ws-server)

(defvar *default-port* 1113)
(defvar *servers* ())

(defclass server (lichat-serverlib:flood-protected-server
                  hunchensocket:websocket-resource)
  ((hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (acceptor :initform NIL :accessor acceptor)
   (thread :initform NIL :accessor thread)
   (ping-interval :initarg :ping-interval :accessor ping-interval)
   (lock :initform (bt:make-recursive-lock) :accessor lock)
   (connections :initform () :accessor connections)
   (connection-limit :initarg :connection-limit :accessor connection-limit))
  (:default-initargs
   :name (machine-instance)
   :hostname "localhost"
   :port *default-port*
   :ping-interval 10
   :connection-limit 100
   :client-class 'connection))

(defclass connection (lichat-serverlib:flood-protected-connection
                      hunchensocket:websocket-client)
  ((lock :initform (bt:make-recursive-lock) :accessor lock)
   (status :initform :starting :accessor status))
  (:default-initargs
   :user NIL))

(defclass channel (lichat-serverlib:channel)
  ((lock :initform (bt:make-recursive-lock) :accessor lock)))

(defclass user (lichat-serverlib:user)
  ((lock :initform (bt:make-recursive-lock) :accessor lock)))

(defmethod lichat-serverlib:make-connection ((server server) &rest initargs)
  (apply #'make-instance 'connection initargs))

(defmethod lichat-serverlib:make-channel ((server server) &rest initargs)
  (apply #'make-instance 'channel initargs))

(defmethod lichat-serverlib:make-user ((server server) &rest initargs)
  (apply #'make-instance 'user initargs))

(defmethod open-connection ((server server))
  (when (thread server)
    (error "Connection thread still exists."))
  (setf (acceptor server) (make-instance 'hunchensocket:websocket-acceptor
                                         :address (hostname server) :port (port server)
                                         :message-log-destination NIL :access-log-destination NIL))
  (hunchentoot:start (acceptor server))
  (v:info :lichat.server.ws "~a: Listening for incoming connections on ~a:~a"
          server (hostname server) (port server))
  (setf (thread server) (bt:make-thread (lambda ()
                                          (unwind-protect
                                               (handle-pings server)
                                            (setf (thread server) NIL)))))
  (pushnew server *servers*)
  server)

(defun dispatch-server (request)
  (dolist (server *servers*)
    (when (eql (hunchentoot:local-port request) (port server))
      (return server))))

(pushnew 'dispatch-server hunchensocket:*websocket-dispatch-table*)

(defmethod close-connection ((server server))
  (unless (acceptor server)
    (error "No connection thread running."))
  (when (thread server)
    (bt:interrupt-thread (thread server) (lambda () (invoke-restart 'stop-handling))))
  (hunchentoot:stop (acceptor server))
  (setf (acceptor server) NIL)
  (setf *servers* (remove server *servers*))
  server)

(defmethod handle-pings ((server server))
  (with-simple-restart (stop-handling "Stop handling pings.")
    (loop with time = (get-universal-time)
          do (sleep 0.1)
             (when (<= (ping-interval server) (- time (get-universal-time)))
               (setf time (get-universal-time))
               (dolist (connection (connections server))
                 (handler-case
                     (when (and (eql :running (status connection))
                                (<= (ping-interval server)
                                    (- (get-universal-time) (lichat-serverlib:last-update connection))))
                       (lichat-serverlib:send! connection 'ping))
                   (error (err)
                     (v:warn :lichat.server.ws err)))))))
  (v:info :lichat.server.ws "Ping handling has stopped."))

(defmethod hunchensocket:client-connected ((server server) (connection connection))
  (v:info :lichat.server.ws "~a: Establishing connection..." server)
  (setf (lichat-serverlib:server connection) server)
  (cond ((<= (connection-limit server) (length (connections server)))
         (lichat-serverlib:send! connection 'too-many-connections)
         (ignore-errors (hunchensocket:close-connection connection :reason "Too many connections.")))
        (T
         (bt:with-recursive-lock-held ((lock server))
           (push connection (connections server))))))

(defmethod hunchensocket:client-disconnected ((server server) (connection connection))
  (ignore-errors (lichat-serverlib:teardown-connection connection))
  (v:info :lichat.server.ws "~a: Closing." connection)
  (bt:with-recursive-lock-held ((lock server))
    (setf (connections server) (remove connection (connections server)))))

(defmethod hunchensocket:text-message-received ((server server) (connection connection) message)
  (handler-case
      (with-input-from-string (in message)
        (case (status connection)
          (:starting
           (handler-case
               (let ((message (lichat-protocol:from-wire in)))
                 (unless (typep message 'lichat-protocol:connect)
                   (error "Expected CONNECT update."))
                 (lichat-serverlib:process connection message)
                 (setf (status connection) :running))
             (lichat-protocol:wire-condition (err)
               (lichat-serverlib:send! connection 'malformed-update
                                       :text (princ-to-string err))
               (lichat-serverlib:teardown-connection connection))))
          (:running
           (lichat-serverlib:process connection in))))
    (error (err)
      (v:error :lichat.server.ws err)
      (lichat-serverlib:send! connection 'failure
                              :text (princ-to-string err))
      (lichat-serverlib:teardown-connection connection))))

(defmethod lichat-serverlib:send ((object lichat-protocol:wire-object) (connection connection))
  (let ((message (with-output-to-string (output) (lichat-protocol:to-wire object output))))
    (bt:with-recursive-lock-held ((lock connection))
      (handler-case (hunchensocket:send-text-message connection message)
        (error (err)
          (v:error :lichat.server.ws err)
          (lichat-serverlib:teardown-connection connection))))))

(defmethod lichat-serverlib:teardown-connection :after ((connection connection))
  (unless (eql :stopping (status connection))
    (setf (status connection) :stopping)
    (ignore-errors (hunchensocket:close-connection connection :reason "Disconnect"))))

;;; Handle synchronising
;; FIXME: I'm not entirely convinced the mutual exclusion
;;        implemented in this model is entirely correct.

(defmethod lichat-serverlib:init-connection :around ((connection connection) update)
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (let ((user (lichat-serverlib:find-user (lichat-protocol:from update)
                                            (lichat-serverlib:server connection))))
      (if user
          (bt:with-recursive-lock-held ((lock user))
            (call-next-method))
          (call-next-method)))))

(defmethod lichat-serverlib:teardown-connection :around ((connection connection))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (let ((user (lichat-protocol:user connection)))
      (if user
          (bt:with-recursive-lock-held ((lock user))
            (call-next-method))
          (call-next-method)))))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:register))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:create))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:join :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-recursive-lock-held ((lock user))
    (bt:with-recursive-lock-held ((lock channel))
      (call-next-method))))

(defmethod lichat-serverlib:leave :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &key id notify-self)
  (declare (ignore id notify-self))
  (bt:with-recursive-lock-held ((lock user))
    (bt:with-recursive-lock-held ((lock channel))
      (call-next-method))))
