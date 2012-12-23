(require :cl-irc)
(require :cl+ssl)
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
   (ssl :col-type boolean :initarg :ssl :accessor server-ssl-p)
   (username :col-type string :initarg :username :accessor server-username)
   (nickname :col-type string :initarg :nickname :accessor server-nickname)
   (current_nickname :col-type string :initarg :nickname :accessor server-current-nickname)
   (realname :col-type string :initarg :nickname :accessor server-realname)
   (connected :col-type boolean :accessor server-connected-p)
   (connection :accessor server-connection))
  (:metaclass postmodern:dao-class)
  (:table-name servers)
  (:keys id))

(defclass channel ()
  ;; XXX get-dao for reader?
  ((id :col-type integer :reader channel-id)
   (server :col-type integer :accessor channel-server)
   (name :col-type text :accessor channel-name)
   (password :col-type text :accessor channel-password)
   (names :col-type text[] :accessor channel-names))
  (:metaclass postmodern:dao-class)
  (:table-name channels)
  (:keys id))

(defun server-update-channels (server)
  (let* ((wanted-channels (postmodern:select-dao 'channel (:= 'server (server-id server))))
         (wanted-channels-name (mapcar #'channel-name wanted-channels)))
    ;; PART channels not in the table anymore
    (loop for channel-name being the hash-keys of (cl-irc:channels (server-connection server))
          unless (find channel-name wanted-channels-name :test #'string=)
          do (cl-irc:part (server-connection server) channel-name))
    ;; JOIN new channels
    (loop for channel in wanted-channels
          do (cl-irc:join (server-connection server)
                          (channel-name channel)
                          :password (channel-password channel)))))

(defun server-add-hook (server msgclass hook)
  (cl-irc:add-hook (server-connection server)
                   msgclass
                   (eval `(lambda (msg)
                            (funcall ,hook ,server msg)))))

(defun server-log-msg (server msg)
  (postmodern:execute
   "INSERT INTO logs (server, time, source, command, target, payload) VALUES ($1, TO_TIMESTAMP($2), $3, $4, $5, $6)"
   (server-id server)
   (local-time:timestamp-to-unix (local-time:universal-to-timestamp (cl-irc:received-time msg)))
   (cl-irc:source msg)
   (cl-irc:command msg)
   (car (cl-irc:arguments msg))
   (cadr (cl-irc:arguments msg))))

(defun server-handle-rpl_welcome (server msg)
  ;; This is at least needed to store and update current-nickname
  (destructuring-bind (nickname welcome-msg) (cl-irc:arguments msg)
    (declare (ignore welcome-msg))
    (setf (server-current-nickname server) nickname))
  (setf (server-connected-p server) t)
  (postmodern:update-dao server)
  (server-update-channels server))

(defun server-handle-rpl_endofnames (server msg)
  (destructuring-bind (nickname channel-name text) (cl-irc:arguments msg)
    (declare (ignore nickname text))
    (let ((channel-dao (car (postmodern:select-dao 'channel (:and (:= 'name channel-name)
                                                                  (:= 'server (server-id server))))))
          (users (loop for user being the hash-values of
                       (cl-irc:users (cl-irc:find-channel (server-connection server) channel-name))
                       collect (cl-irc:nickname user))))
      (setf (channel-names channel-dao)
            (make-array (length users) :initial-contents users))
      (postmodern:update-dao channel-dao))))

(defun server-handle-nick (server msg)
  "Handle nick change."
  (destructuring-bind (new-nick) (cl-irc:arguments msg)
    (setf (server-current-nickname server) new-nick))
  (postmodern:update-dao server))

(defun server-handle-err_nicknameinuse-message (server msg)
  "Handle nick already in used error."
  ;; Try to change the nick to "current-nickname + _"
  (destructuring-bind (_ tried-nickname error) (cl-irc:arguments msg)
    (declare (ignore _ error))
    (cl-irc:nick (server-connection server)
                 (format nil "~a_" tried-nickname))))

(defun server-connect (server)
  "Connect to the server."
  (setf (server-connection server)
        (cl-irc:connect :nickname (server-nickname server)
                        :server (format nil "~a" (server-address server))
                        :port (server-port server)
                        :connection-security (when (server-ssl-p server)
                                               :ssl)
                        :realname (server-realname server)))

  ;; Update (clean) the server database in the database
  (setf (server-current-nickname server) "")
  (setf (server-connected-p server) nil)
  (postmodern:update-dao server)

  (server-add-hook server
                   'cl-irc:irc-err_nicknameinuse-message
                   #'server-handle-err_nicknameinuse-message)
  (server-add-hook server 'cl-irc:irc-rpl_welcome-message #'server-handle-rpl_welcome)
  (server-add-hook server 'cl-irc:irc-rpl_endofnames-message #'server-handle-rpl_endofnames)
  (server-add-hook server 'cl-irc:irc-nick-message #'server-handle-nick)
  (dolist (msg-type '(privmsg notice kick topic error mode nick join part quit kill invite))
    (server-add-hook server
                     (intern (format nil "IRC-~a-MESSAGE" msg-type) "IRC")
                     #'server-log-msg)))

(defun server-listen (server)
  (postmodern:execute (concatenate 'string "LISTEN channel_" (write-to-string (server-id server))))
  (loop while (cl-postgres:wait-for-notification postmodern:*database*)
        do (server-update-channels server)))

(let ((server (car (postmodern:select-dao 'server "true LIMIT 1"))))
  (server-connect server)
  (bordeaux-threads:make-thread (lambda ()
                                  (let ((postmodern:*database* *database-irc*))
                                    (cl-irc:read-message-loop (server-connection server))))
                                :name "irc-read-message-loop")
  (server-listen server))
