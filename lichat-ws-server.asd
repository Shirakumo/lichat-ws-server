#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem lichat-ws-server
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple WebSocket server implementation for lichat."
  :homepage "https://Shirakumo.github.io/lichat-ws-server/"
  :bug-tracker "https://github.com/Shirakumo/lichat-ws-server/issues"
  :source-control (:git "https://github.com/Shirakumo/lichat-ws-server.git")
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "documentation"))
  :depends-on (:lichat-protocol
               :lichat-serverlib
               :hunchensocket
               :bordeaux-threads
               :documentation-utils
               :verbose))
