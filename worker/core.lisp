(require :cl-irc)
(require :cl+ssl)
(require :bordeaux-threads)
(require :postmodern)
(require :cl-postgres)
(require :simple-date)
(require :local-time)
(require :cl-async)
(require :cl-async-ssl)

;; MAIN

(postmodern:connect-toplevel "kawoosh" "kawoosh" "kawoosh" "localhost")
(postmodern:execute "SET TIMEZONE='UTC'")

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
   (motd :col-type text :accessor server-motd)
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
   (names :col-type text[] :accessor channel-names)
   (modes :col-type text[] :accessor channel-modes)
   (topic :col-type text :accessor channel-topic)
   (topic_who :col-type text :accessor channel-topic-who)
   (topic_time :col-type timestamp :accessor channel-topic-time)
   (creation_time :col-type timestamp :accessor channel-creation-time))
  (:metaclass postmodern:dao-class)
  (:table-name channels)
  (:keys id))

(defvar *stream->connection* (make-hash-table))

(defun server-socket-read (socket stream)
  (declare (ignore socket))
  (loop with connection = (gethash stream *stream->connection*)
        for message = (cl-irc:read-irc-message connection)
        while message
        do (cl-irc:irc-message-event connection message)))

(defun server-socket-event (stuff)
  (format t "STUFF ~a ~%" stuff))

(defun server-connect (server port nickname
                              &key
                              (username nil)
                              (realname nil)
                              (password nil)
                              (ssl nil))
  "Connect to server and return a connection object."
  (let ((connection (cl-irc:make-connection :connection-type 'cl-irc:connection
                                            :network-stream (funcall (if ssl
                                                                         #'cl-async-ssl:tcp-ssl-connect
                                                                       #'cl-async:tcp-connect)
                                                                     server (or port 6667)
                                                                     #'server-socket-read
                                                                     #'server-socket-event
                                                                     :stream t)
                                            :client-stream t
                                            :server-name server)))
    ;; Register the connection
    (setf (gethash (cl-irc:network-stream connection) *stream->connection*)
          connection)

    (cl-irc:add-default-hooks connection)

    (unless (null password)
      (cl-irc:pass connection password))
    (cl-irc:nick connection nickname)
    (cl-irc:user- connection (or username nickname) 0 (or realname nickname))
    connection))

(defun irc-message-received-timestamp (msg)
  "Return the SQL timestamp for msg."
  (simple-date:universal-time-to-timestamp
   (cl-irc:received-time msg)))

(defun irc-user-serialize (user)
  (format nil "~a!~a@~a"
          (cl-irc:nickname user)
          (cl-irc:username user)
          (cl-irc:hostname user)))

(defun server-update-channels (server)
  ;; Check that connection has been set (= connected) otherwise don't do anything.
  (when (slot-boundp server 'connection)
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
                            :password (channel-password channel))
            ;; Send an empty mode command to retrieve channel mode and
            ;; eventually creation time
            do (cl-irc:mode (server-connection server)
                            (channel-name channel))))))

(defun server-add-hook (server msgclass hook &optional last)
  "Add a server hook for msgclass.
If last is not nil, put the hook in the last run ones."
  (funcall (if last
               #'cl-irc:append-hook
             #'cl-irc:add-hook)
           (server-connection server)
           msgclass
           (eval `(lambda (msg)
                    (funcall ,hook ,server msg)))))

(defun server-log-msg (server msg)
  (postmodern:execute
   "INSERT INTO logs (server, time, source, command, target, payload) VALUES ($1, $2, $3, $4, $5, $6)"
   (server-id server)
   (irc-message-received-timestamp msg)
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

(defun server-handle-rpl_motd (server msg)
  (destructuring-bind (target text) (cl-irc:arguments msg)
    (declare (ignore target))
    (setf (server-motd server)
          (format nil "~a~a~%"
                  (let ((motd (server-motd server)))
                    (if (eq motd :null) "" motd))
                  text))))

(defun server-handle-rpl_endofmotd (server msg)
  (destructuring-bind (target text) (cl-irc:arguments msg)
    (declare (ignore target text))
    (postmodern:update-dao server)))

;; XXX unit test me
;; XXX move this into a utility package
(defun list->array (l)
  "Recursive list to array conversion."
  (make-array (length l) :initial-contents l))

(defun channel-update-names (server channel)
  (let ((users (loop for user being the hash-values of
                     (cl-irc:users (cl-irc:find-channel (server-connection server)
                                                        (channel-name channel)))
                     collect (irc-user-serialize user))))
    (setf (channel-names channel)
          (if users (list->array users) :null))
    (postmodern:update-dao channel)))

(defun irc-channel-serialize-mode (channel)
   (loop for (mode raw-value) on (cl-irc:modes channel) by #'cddr
         for value = (cl-irc:get-mode channel mode)
         if value
         collect (format nil "~a~@[~a~]"
                         mode
                         (cond ((listp value)
                                (format nil " ~{~a~^~}"
                                        ;; There might be list of other
                                        ;; things, but so far in channels, I
                                        ;; don't see that's true.
                                        (mapcar #'irc-user-serialize value)))
                               ((stringp value)
                                (format nil " ~a" value))))))

(defun channel-update-mode (server channel)
  (setf (channel-modes channel)
        (let ((modes (irc-channel-serialize-mode (cl-irc:find-channel (server-connection server)
                                                                      (channel-name channel)))))
            (if modes
                (list->array modes)
              :null)))
  (postmodern:update-dao channel))

(defun channel-find (server channel-name)
  (car (postmodern:select-dao 'channel (:and (:= 'name channel-name)
                                             (:= 'server (server-id server))))))

(defun server-handle-join (server msg)
  (unless (cl-irc:self-message-p msg)
    (destructuring-bind (channel-name) (cl-irc:arguments msg)
      (channel-update-names server (channel-find server channel-name)))))

(defun server-handle-part (server msg)
  (unless (cl-irc:self-message-p msg)
    (destructuring-bind (channel-name &optional text) (cl-irc:arguments msg)
      (declare (ignore text))
      (channel-update-names server (channel-find server channel-name)))))

(defun server-handle-quit (server msg)
  (declare (ignore msg))
  ;; TODO this can be optimized by only iterating on channels that have the
  ;; user (aka `(cl-irc:source msg)') in the channel.names array.
  (dolist (channel (postmodern:select-dao 'channel (:= 'server (server-id server))))
    (channel-update-names server channel)))

(defun server-handle-rpl_endofnames (server msg)
  (destructuring-bind (nickname channel-name text) (cl-irc:arguments msg)
    (declare (ignore nickname text))
    (let ((channel (channel-find server channel-name)))
      ;; TODO only one DAO update please
      (channel-update-names server channel)
      (channel-update-mode server channel))))

(defun server-handle-rpl_channelmodeis (server msg)
  (destructuring-bind (target channel &rest mode-arguments) (cl-irc:arguments msg)
    (declare (ignore target mode-arguments))
    (channel-update-mode server (channel-find server channel))))

(defun server-handle-mode (server msg)
  (destructuring-bind (target &rest mode-arguments) (cl-irc:arguments msg)
    (declare (ignore mode-arguments))
    (let ((channel (channel-find server target)))
      (when channel
        (channel-update-mode server channel)))))

(defun server-handle-rpl_topic (server msg)
  (destructuring-bind (target channel-name &optional topic) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find server channel-name)))
      (setf (channel-topic channel) topic)
      (postmodern:update-dao channel))))

(defun server-handle-topic (server msg)
  (destructuring-bind (channel-name &optional topic) (cl-irc:arguments msg)
    (let ((channel (channel-find server channel-name)))
      (setf (channel-topic channel) topic)
      (setf (channel-topic-who channel) (cl-irc:source msg))
      (setf (channel-topic-time channel) (irc-message-received-timestamp msg))
      (postmodern:update-dao channel))))

;; XXX move to util
(defun unix-time->timestamp (unix-time)
  (simple-date:universal-time-to-timestamp
   (local-time:timestamp-to-universal
    (local-time:unix-to-timestamp unix-time))))

(defun server-handle-rpl_topicwhotime (server msg)
  (destructuring-bind (target channel-name who time) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find server channel-name)))
      (setf (channel-topic-who channel) who)
      (setf (channel-topic-time channel) (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

(defun server-handle-rpl_creationtime (server msg)
  (destructuring-bind (target channel-name time) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find server channel-name)))
      (setf (channel-creation-time channel)
            (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

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

(defun server-run (server)
  "Connect to the server."
  (setf (server-connection server)
        (server-connect (format nil "~a" (server-address server))
                        (server-port server)
                        (server-nickname server)
                        :ssl (server-ssl-p server)
                        :realname (server-realname server)))

  ;; Update (clean) the server database in the database
  (setf (server-current-nickname server) :null)
  (setf (server-connected-p server) nil)
  (setf (server-motd server) :null)
  (postmodern:update-dao server)

  (dolist (channel (postmodern:select-dao 'channel (:= 'server (server-id server))))
    (setf (channel-names channel) :null)
    (setf (channel-modes channel) :null)
    (setf (channel-topic channel) :null)
    (setf (channel-topic-who channel) :null)
    (setf (channel-topic-time channel) :null)
    (setf (channel-creation-time channel) :null)
    (postmodern:update-dao channel))

  (server-add-hook server
                   'cl-irc:irc-err_nicknameinuse-message
                   #'server-handle-err_nicknameinuse-message)

  ;; These hooks have to be last to be ran after cl-irc internally updated
  ;; its copy of users in connection/channels
  (server-add-hook server 'cl-irc:irc-join-message #'server-handle-join t)
  (server-add-hook server 'cl-irc:irc-part-message #'server-handle-part t)
  (server-add-hook server 'cl-irc:irc-quit-message #'server-handle-quit t)
  (server-add-hook server 'cl-irc:irc-rpl_endofnames-message #'server-handle-rpl_endofnames t)
  (server-add-hook server 'cl-irc:irc-rpl_channelmodeis-message #'server-handle-rpl_channelmodeis t)
  (server-add-hook server 'cl-irc:irc-mode-message #'server-handle-mode t)

  ;; Channel topic handling
  (server-add-hook server 'cl-irc:irc-topic-message #'server-handle-topic)
  (server-add-hook server 'cl-irc:irc-rpl_topic-message #'server-handle-rpl_topic)
  (server-add-hook server 'cl-irc:irc-rpl_topicwhotime-message #'server-handle-rpl_topicwhotime)

  (server-add-hook server 'cl-irc:irc-rpl_creationtime-message #'server-handle-rpl_creationtime)

  (server-add-hook server 'cl-irc:irc-rpl_welcome-message #'server-handle-rpl_welcome)
  (server-add-hook server 'cl-irc:irc-rpl_motd-message #'server-handle-rpl_motd)
  (server-add-hook server 'cl-irc:irc-rpl_endofmotd-message #'server-handle-rpl_endofmotd)
  (server-add-hook server 'cl-irc:irc-nick-message #'server-handle-nick)
  (dolist (msg-type '(privmsg notice kick topic error mode nick join part quit kill invite))
    (server-add-hook server
                     (intern (format nil "IRC-~a-MESSAGE" msg-type) "IRC")
                     #'server-log-msg)))

(defun server-listen (server)
  (postmodern:execute (concatenate 'string "LISTEN channel_" (write-to-string (server-id server))))
  (loop while (cl-postgres:wait-for-notification postmodern:*database*)
        do (server-update-channels server)))

(defun start ()
  (let ((server (car (postmodern:select-dao 'server "true LIMIT 1"))))
    (bordeaux-threads:make-thread (lambda ()
                                    (server-listen server))
                                  :name "server-listen")
    (let ((postmodern:*database* *database-irc*))
      (postmodern:execute "SET TIMEZONE='UTC'")
      (server-run server))))

(cl-async:start-event-loop #'start)
