(asdf:defsystem lichat-ws-server
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
