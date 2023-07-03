(in-package #:org.shirakumo.lichat.ws-server)

(docs:define-docs
  (variable *default-port*
    "The standard port on which the server will run.

Should be 1113.")

  (type server
    "Server class. You should instantiate this.

Access to the server is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following updates and processing methods for updates
are mutually excluded via this lock:

 LICHAT-SERVERLIB:TEARDOWN-CONNECTION
 LICHAT-PROTOCOL:CONNECT
 LICHAT-PROTOCOL:REGISTER
 LICHAT-PROTOCOL:CREATE

See LICHAT-SERVERLIB:FLOOD-PROTECTED-SERVER
See HUNCHENSOCKET:WEBSOCKET-RESOURCE
See HOSTNAME
See PORT
See ACCEPTOR
See THREAD
See PING-INTERVAL
See LOCK
See CONNECTIONS")

  (function hostname
    "Accessor to the hostname of the object.

See SERVER")

  (function port
    "Accessor to the port of the object.

See SERVER
See *DEFAULT-PORT*")

  (function acceptor
    "Accessor to the Hunchentoot acceptor of the server.

See SERVER")

  (function thread
    "Accessor to the background ping thread of the server.

See SERVER")

  (function ping-interval
    "Accessor to the amount of seconds to wait for an update before sending a PING.

See SERVER")

  (function lock
    "Accessor to the lock of the object that is used to mutually exclude access.

See SERVER 
See CONNECTION
See USER
See CHANNEL")

  (function connections
    "Accessor to the list of connections on the server.

See SERVER
See CONNECTION")

  (type connection
    "Connection class. Each connection to a client will have an instance of this class.

Access to the connection is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:SEND

See LICHAT-SERVERLIB:FLOOD-PROTECTED-CONNECTION
See HUNCHENSOCKET:WEBSOCKET-CLIENT
See STATUS
See LOCK")

  (function status
    "Accessor to the status of the connection.

The value should be one of :STARTING :RUNNING :STOPPING

See CONNECTION")

  (type channel
    "Channel class.

Access to the channel is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:JOIN
  LICHAT-SERVERLIB:LEAVE

See LICHAT-SERVERLIB:CHANNEL
See LOCK")

  (type user
    "User class.

Access to the user is properly handled for
mutual exclusion from threads by its LOCK. Particularly,
the following methods are mutually excluded via this lock:

  LICHAT-SERVERLIB:JOIN
  LICHAT-SERVERLIB:LEAVE

See LICHAT-SERVERLIB:USER
See LOCK")

  (function open-connection
    "Start accepting incoming connections on the server.

This will launch a background thread which will call
HANDLE-PINGS on the server. It will also construct
and start the Hunchentoot acceptor of the server.

See HANDLE-PINGS
See THREAD
See ACCEPTOR
See SERVER")

  (function close-connection
    "Stop accepting incoming connections and close all existing ones.

See CONNECTIONS
See THREAD
See SERVER")

  (function handle-pings
    "This function will loop on the server and send ping requests in a regular interval to clients that have not replied in a while.

During the processing a restart called STOP-HANDLING
is active that, when invoked, exits the function.

See PING-INTERVAL
See SERVER"))
