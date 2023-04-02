#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-ws-server
  (:nicknames #:org.shirakumo.lichat.ws-server)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:use #:cl)
  (:export
   #:*default-port*
   #:server
   #:hostname
   #:port
   #:thread
   #:ping-interval
   #:lock
   #:connections
   #:connection-limit
   #:connection
   #:status
   #:lock
   #:channel
   #:lock
   #:user
   #:lock
   #:open-connection
   #:close-connection
   #:handle-pings))
