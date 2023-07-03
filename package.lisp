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
