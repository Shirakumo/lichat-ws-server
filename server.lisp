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
   (lock :initform (bt:make-lock) :accessor lock)
   (connections :initform () :accessor connections)
   (connection-limit :initarg :connection-limit :accessor connection-limit))
  (:default-initargs
   :name (machine-instance)
   :hostname "localhost"
   :port *default-port*
   :ping-interval 60
   :connection-limit 100
   :client-class 'connection))

(defclass connection (lichat-serverlib:flood-protected-connection
                      hunchensocket:websocket-client)
  ((lock :initform (bt:make-lock) :accessor lock)
   (status :initform :starting :accessor status))
  (:default-initargs
   :user NIL))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type T)
    (format stream "~a/~a"
            (when (lichat-serverlib:server connection) (lichat-protocol:name (lichat-serverlib:server connection)))
            (when (lichat-protocol:user connection) (lichat-protocol:name (lichat-protocol:user connection))))))

(defclass channel (lichat-serverlib:channel)
  ((lock :initform (bt:make-lock) :accessor lock)))

(defclass user (lichat-serverlib:user)
  ((lock :initform (bt:make-lock) :accessor lock)))

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
    (loop for counter = 1 then (mod (1+ counter) (* 10 (ping-interval server)))
          do (sleep 0.1)
             (when (= 0 counter)
               (dolist (connection (connections server))
                 (when (and (eql :running (status connection))
                            (<= (ping-interval server)
                                (- (get-universal-time) (lichat-serverlib:last-update connection))))
                   (lichat-serverlib:send! connection 'ping)))))))

(defmethod hunchensocket:client-connected ((server server) (connection connection))
  (v:info :lichat.server "~a: Establishing connection..." server)
  (setf (lichat-serverlib:server connection) server)
  (cond ((<= (connection-limit server) (length (connections server)))
         (lichat-serverlib:send! connection 'too-many-connections)
         (ignore-errors (hunchensocket:close-connection connection :reason "Too many connections.")))
        (T
         (push connection (connections server)))))

(defmethod hunchensocket:client-disconnected ((server server) (connection connection))
  (lichat-serverlib:teardown-connection connection))

(defmethod hunchensocket:text-message-received ((server server) (connection connection) message)
  (v:trace :test "~a: Handling ~s" connection message)
  (restart-case
      (with-input-from-string (in message)
        (case (status connection)
          (:running
           (lichat-serverlib:process connection in))
          (:starting
           (handler-case
               (let ((message (lichat-protocol:from-wire in)))
                 (etypecase message
                   (lichat-protocol:connect
                    (lichat-serverlib:process connection message)))
                 (setf (status connection) :running))
             (lichat-protocol:wire-condition (err)
               (lichat-serverlib:send! connection 'malformed-update
                                       :text (princ-to-string err))
               (invoke-restart 'lichat-serverlib:close-connection))
             (error (err)
               (lichat-serverlib:send! connection 'failure
                                       :text (princ-to-string err))
               (invoke-restart 'lichat-serverlib:close-connection))))))
    (lichat-serverlib:close-connection ()
      :report "Close the connection."
      (lichat-serverlib:teardown-connection connection))))

(defmethod (setf lichat-serverlib:find-channel) :before (channel name (server server))
  (v:info :lichat.server "~a: Creating channel ~a" server channel))

(defmethod (setf lichat-serverlib:find-user) :before (user name (server server))
  (v:info :lichat.server "~a: Creating user ~a" server user))

(defmethod (setf lichat-serverlib:find-profile) :before (profile name (server server))
  (v:info :lichat.server "~a: Creating profile ~a" server profile))

(defmethod lichat-serverlib:teardown-connection :after ((connection connection))
  (unless (eql (status connection) :stopping)
    (v:info :lichat.server "~a: Closing ~a" (lichat-serverlib:server connection) connection)
    (setf (status connection) :stopping)
    (ignore-errors (hunchensocket:close-connection connection :reason "Disconnect"))
    (setf (connections server) (remove connection (connections server)))))

(defmethod lichat-serverlib:send ((object lichat-protocol:wire-object) (connection connection))
  (v:trace :lichat.server "~a: Sending ~s to ~a" (lichat-serverlib:server connection) object connection)
  (let ((message (with-output-to-string (output)
                   (lichat-protocol:to-wire object output))))
    (bt:with-lock-held ((lock connection))
      (hunchensocket:send-text-message connection message))))

;;; Handle synchronising
;; FIXME: I'm not entirely convinced the mutual exclusion
;;        implemented in this model is entirely correct.

;; OPs that need a global lock
(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:connect))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:teardown-connection :around ((connection connection))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:register))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:create))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

;; OPs that need a local lock
(defmethod lichat-serverlib:join :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-lock-held ((lock channel))
    (bt:with-lock-held ((lock user))
      (call-next-method))))

(defmethod lichat-serverlib:leave :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-lock-held ((lock channel))
    (bt:with-lock-held ((lock user))
      (call-next-method))))
