(require :cl-irc)
(require :bordeaux-threads)
(require :postmodern)
(require :cl-postgres)
(require :local-time)

;; MAIN

(postmodern:connect-toplevel "kawoosh" "kawoosh" "kawoosh" "localhost")

(defvar *database-irc*
  (postmodern:connect "kawoosh" "kawoosh" "kawoosh" "localhost"))

(defclass server ()
  ((id :col-type integer :reader server-id)
   (name :col-type string :initarg :name :accessor server-name)
   (address :col-type string :initarg :address :accessor server-address)
   (port :col-type integer :initarg :port :accessor server-port)
   (username :col-type string :initarg :username :accessor server-username)
   (nickname :col-type string :initarg :nickname :accessor server-nickname)
   (realname :col-type string :initarg :nickname :accessor server-realname)
   (connection :accessor server-connection))
  (:metaclass postmodern:dao-class)
  (:table-name servers)
  (:keys id))

(defclass channel ()
  ;; XXX get-dao for reader?
  ((id :col-type integer :reader channel-id)
   (server :col-type integer :accessor channel-server)
   (name :col-type text :accessor channel-name)
   (password :col-type text :accessor channel-password))
  (:metaclass postmodern:dao-class)
  (:table-name channels)
  (:keys id))

(defun server-connect (server)
  (setf (server-connection server)
        (cl-irc:connect :nickname (server-nickname server)
                        :server (format nil "~a" (server-address server))
                        :port (server-port server)
                        :realname (server-realname server)))
  (let ((hook (eval `(lambda (msg)
                       (server-log ,server msg)))))
    (dolist (msg-type '(privmsg notice kick topic error mode nick join part quit kill invite))
      (cl-irc:add-hook (server-connection server)
                       (intern (format nil "IRC-~a-MESSAGE" msg-type) "IRC") hook))))

(defun server-join (server)
  (dolist (channel (postmodern:select-dao 'channel (:= 'server (server-id server))))
    (cl-irc:join (server-connection server)
                 (channel-name channel)
                 :password (channel-password channel))))

(defun server-listen (server)
  (postmodern:execute (concatenate 'string "LISTEN channel_" (write-to-string (server-id server))))
  (loop while (cl-postgres:wait-for-notification postmodern:*database*)
        do (server-join server)))

(defun server-log (server msg)
  (postmodern:execute
   "INSERT INTO logs (server, time, source, command, target, payload) VALUES ($1, TO_TIMESTAMP($2), $3, $4, $5, $6)"
   (server-id server)
   (local-time:timestamp-to-unix (local-time:universal-to-timestamp (cl-irc:received-time msg)))
   (cl-irc:source msg)
   (cl-irc:command msg)
   (car (cl-irc:arguments msg))
   (cadr (cl-irc:arguments msg))))


(let ((server (car (postmodern:select-dao 'server "true LIMIT 1"))))
  (server-connect server)
  (server-join server)
  (bordeaux-threads:make-thread (lambda ()
                                  (let ((postmodern:*database* *database-irc*))
                                    (cl-irc:read-message-loop (server-connection server))))
                                :name "irc-read-message-loop")
  (server-listen server))
