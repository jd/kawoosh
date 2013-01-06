(require :cl-irc)
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

(defclass server ()
  ((name :col-type string :initarg :name :accessor server-name)
   (address :col-type string :initarg :address :accessor server-address)
   (port :col-type integer :initarg :port :accessor server-port)
   (ssl :col-type boolean :initarg :ssl :accessor server-ssl-p))
  (:metaclass postmodern:dao-class)
  (:table-name servers)
  (:keys name))

(defclass connection ()
  ((id :col-type serial :reader connection-id)
   (server :col-type text :initarg :server :accessor connection-server)
   (username :col-type string :initarg :username :accessor connection-username)
   (nickname :col-type string :initarg :nickname :accessor connection-nickname)
   (current_nickname :col-type string :initarg :nickname :accessor connection-current-nickname)
   (realname :col-type string :initarg :nickname :accessor connection-realname)
   (connected :col-type boolean :accessor connection-connected-p)
   (motd :col-type text :accessor connection-motd)
   (network-connection :initform nil :accessor connection-network-connection))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass channel ()
  ((id :col-type serial :reader channel-id)
   (connection :col-type serial :accessor channel-connection)
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

(defun connection-socket-read (socket stream)
  (declare (ignore socket))
  (loop with connection = (gethash stream *stream->connection*)
        for message = (cl-irc:read-irc-message connection)
        while message
        do (cl-irc:irc-message-event connection message)))

(defun connection-socket-event (ev)
  (format t "Socket event: ~a~%" ev))

(defun connection-connect (server port nickname
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
                                                                     server port
                                                                     #'connection-socket-read
                                                                     #'connection-socket-event
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

(defun connection-update-channels (connection)
  ;; Check that connection has been set (= connected) otherwise don't do anything.
  (when (and (connection-network-connection connection)
             (cl-irc::connectedp (connection-network-connection connection)))
    (let* ((wanted-channels (postmodern:select-dao 'channel (:= 'connection (connection-id connection))))
           (wanted-channels-name (mapcar #'channel-name wanted-channels)))
      (with-slots (network-connection)
          connection
        ;; PART channels not in the table anymore
        (loop for channel-name being the hash-keys of (cl-irc:channels network-connection)
              unless (find channel-name wanted-channels-name :test #'string=)
                do (cl-irc:part network-connection channel-name))
        ;; JOIN new channels
        (loop for channel in wanted-channels
              do (cl-irc:join network-connection
                              (channel-name channel)
                              :password (channel-password channel))
                 ;; Send an empty mode command to retrieve channel mode and
                 ;; eventually creation time
              do (cl-irc:mode network-connection
                              (channel-name channel)))))))

(defun connection-add-hook (connection msgclass hook &optional last)
  "Add a server hook for msgclass.
If last is not nil, put the hook in the last run ones."
  (funcall (if last
               #'cl-irc:append-hook
             #'cl-irc:add-hook)
           (connection-network-connection connection)
           msgclass
           (eval `(lambda (msg)
                    (funcall ,hook ,connection msg)))))

(defun connection-log-msg (connection msg)
  (postmodern:execute
   "INSERT INTO logs (connection, time, source, command, target, payload) VALUES ($1, $2, $3, $4, $5, $6)"
   (connection-id connection)
   (irc-message-received-timestamp msg)
   (cl-irc:source msg)
   (cl-irc:command msg)
   (car (cl-irc:arguments msg))
   (cadr (cl-irc:arguments msg))))

(defun connection-handle-rpl_welcome (connection msg)
  ;; This is at least needed to store and update current-nickname
  (destructuring-bind (nickname welcome-msg) (cl-irc:arguments msg)
    (declare (ignore welcome-msg))
    (setf (connection-current-nickname connection) nickname))
  (setf (connection-connected-p connection) t)
  (postmodern:update-dao connection)
  (connection-update-channels connection))

(defun connection-handle-rpl_motd (connection msg)
  (destructuring-bind (target text) (cl-irc:arguments msg)
    (declare (ignore target))
    (setf (connection-motd connection)
          (format nil "~a~a~%"
                  (let ((motd (connection-motd connection)))
                    (if (eq motd :null) "" motd))
                  text))))

(defun connection-handle-rpl_endofmotd (connection msg)
  (destructuring-bind (target text) (cl-irc:arguments msg)
    (declare (ignore target text))
    (postmodern:update-dao connection)))

;; XXX unit test me
;; XXX move this into a utility package
(defun list->array (l)
  "Recursive list to array conversion."
  (make-array (length l) :initial-contents l))

(defun channel-update-names (connection channel)
  (let ((users (loop for user being the hash-values of
                     (cl-irc:users (cl-irc:find-channel (connection-network-connection connection)
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

(defun channel-update-mode (connection channel)
  (setf (channel-modes channel)
        (let ((modes (irc-channel-serialize-mode (cl-irc:find-channel (connection-network-connection connection)
                                                                      (channel-name channel)))))
            (if modes
                (list->array modes)
              :null)))
  (postmodern:update-dao channel))

(defun channel-find (connection channel-name)
  (car (postmodern:select-dao 'channel (:and (:= 'name channel-name)
                                             (:= 'connection (connection-id connection))))))

(defun connection-handle-join (connection msg)
  (unless (cl-irc:self-message-p msg)
    (destructuring-bind (channel-name) (cl-irc:arguments msg)
      (channel-update-names connection (channel-find connection channel-name)))))

(defun connection-handle-part (connection msg)
  (unless (cl-irc:self-message-p msg)
    (destructuring-bind (channel-name &optional text) (cl-irc:arguments msg)
      (declare (ignore text))
      (channel-update-names connection (channel-find connection channel-name)))))

(defun connection-handle-quit (connection msg)
  (declare (ignore msg))
  ;; TODO this can be optimized by only iterating on channels that have the
  ;; user (aka `(cl-irc:source msg)') in the channel.names array.
  (dolist (channel (postmodern:select-dao 'channel (:= 'connection (connection-id connection))))
    (channel-update-names connection channel)))

(defun connection-handle-rpl_endofnames (connection msg)
  (destructuring-bind (nickname channel-name text) (cl-irc:arguments msg)
    (declare (ignore nickname text))
    (let ((channel (channel-find connection channel-name)))
      ;; TODO only one DAO update please
      (channel-update-names connection channel)
      (channel-update-mode connection channel))))

(defun connection-handle-rpl_channelmodeis (connection msg)
  (destructuring-bind (target channel &rest mode-arguments) (cl-irc:arguments msg)
    (declare (ignore target mode-arguments))
    (channel-update-mode connection (channel-find connection channel))))

(defun connection-handle-mode (connection msg)
  (destructuring-bind (target &rest mode-arguments) (cl-irc:arguments msg)
    (declare (ignore mode-arguments))
    (let ((channel (channel-find connection target)))
      (when channel
        (channel-update-mode connection channel)))))

(defun connection-handle-rpl_topic (connection msg)
  (destructuring-bind (target channel-name &optional topic) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic channel) topic)
      (postmodern:update-dao channel))))

(defun connection-handle-topic (connection msg)
  (destructuring-bind (channel-name &optional topic) (cl-irc:arguments msg)
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic channel) topic)
      (setf (channel-topic-who channel) (cl-irc:source msg))
      (setf (channel-topic-time channel) (irc-message-received-timestamp msg))
      (postmodern:update-dao channel))))

;; XXX move to util
(defun unix-time->timestamp (unix-time)
  (simple-date:universal-time-to-timestamp
   (local-time:timestamp-to-universal
    (local-time:unix-to-timestamp unix-time))))

(defun connection-handle-rpl_topicwhotime (connection msg)
  (destructuring-bind (target channel-name who time) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic-who channel) who)
      (setf (channel-topic-time channel) (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

(defun connection-handle-rpl_creationtime (connection msg)
  (destructuring-bind (target channel-name time) (cl-irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-creation-time channel)
            (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

(defun connection-handle-nick (connection msg)
  "Handle nick change."
  (destructuring-bind (new-nick) (cl-irc:arguments msg)
    (setf (connection-current-nickname connection) new-nick))
  (postmodern:update-dao connection))

(defun connection-handle-err_nicknameinuse-message (connection msg)
  "Handle nick already in used error."
  ;; Try to change the nick to "current-nickname + _"
  (destructuring-bind (_ tried-nickname error) (cl-irc:arguments msg)
    (declare (ignore _ error))
    (cl-irc:nick (connection-network-connection connection)
                 (format nil "~a_" tried-nickname))))

(defun connection-run (connection)
  "Connect to the connection."
  (let ((server (car (postmodern:select-dao 'server (:= 'name (connection-server connection))))))
    (setf (connection-network-connection connection)
          (connection-connect (format nil "~a" (server-address server))
                              (server-port server)
                              (connection-nickname connection)
                              :ssl (server-ssl-p server)
                              :realname (connection-realname connection))))

  ;; Update (clean) the connection entry in the database
  (setf (connection-current-nickname connection) :null)
  (setf (connection-connected-p connection) nil)
  (setf (connection-motd connection) :null)
  (postmodern:update-dao connection)

  (dolist (channel (postmodern:select-dao 'channel (:= 'connection (connection-id connection))))
    (setf (channel-names channel) :null)
    (setf (channel-modes channel) :null)
    (setf (channel-topic channel) :null)
    (setf (channel-topic-who channel) :null)
    (setf (channel-topic-time channel) :null)
    (setf (channel-creation-time channel) :null)
    (postmodern:update-dao channel))

  (connection-add-hook connection
                   'cl-irc:irc-err_nicknameinuse-message
                   #'connection-handle-err_nicknameinuse-message)

  ;; These hooks have to be last to be ran after cl-irc internally updated
  ;; its copy of users in connection/channels
  (connection-add-hook connection 'cl-irc:irc-join-message #'connection-handle-join t)
  (connection-add-hook connection 'cl-irc:irc-part-message #'connection-handle-part t)
  (connection-add-hook connection 'cl-irc:irc-quit-message #'connection-handle-quit t)
  (connection-add-hook connection 'cl-irc:irc-rpl_endofnames-message #'connection-handle-rpl_endofnames t)
  (connection-add-hook connection 'cl-irc:irc-rpl_channelmodeis-message #'connection-handle-rpl_channelmodeis t)
  (connection-add-hook connection 'cl-irc:irc-mode-message #'connection-handle-mode t)

  ;; Channel topic handling
  (connection-add-hook connection 'cl-irc:irc-topic-message #'connection-handle-topic)
  (connection-add-hook connection 'cl-irc:irc-rpl_topic-message #'connection-handle-rpl_topic)
  (connection-add-hook connection 'cl-irc:irc-rpl_topicwhotime-message #'connection-handle-rpl_topicwhotime)

  (connection-add-hook connection 'cl-irc:irc-rpl_creationtime-message #'connection-handle-rpl_creationtime)

  (connection-add-hook connection 'cl-irc:irc-rpl_welcome-message #'connection-handle-rpl_welcome)
  (connection-add-hook connection 'cl-irc:irc-rpl_motd-message #'connection-handle-rpl_motd)
  (connection-add-hook connection 'cl-irc:irc-rpl_endofmotd-message #'connection-handle-rpl_endofmotd)
  (connection-add-hook connection 'cl-irc:irc-nick-message #'connection-handle-nick)
  (dolist (msg-type '(privmsg notice kick topic error mode nick join part quit kill invite))
    (connection-add-hook connection
                     (intern (format nil "IRC-~a-MESSAGE" msg-type) "IRC")
                     #'connection-log-msg)))


(defvar *notification-lock* (bt:make-lock "notifications"))
(defvar *notifications* nil)

(defun connection-listen (connection)
  (postmodern:execute (concatenate 'string "LISTEN channel_" (write-to-string (connection-id connection))))
  (loop for (channel payload pid) = (multiple-value-list
                                     (cl-postgres:wait-for-notification postmodern:*database*))
        do (bt:with-lock-held (*notification-lock*)
             ;; XXX Don't push if already there
             (pushnew (list channel payload) *notifications*
                      :test #'equal))))

(defun notification-handler (connection)
  (bt:with-lock-held (*notification-lock*)
    (loop for (channel payload) in *notifications*
          do (connection-update-channels connection))
    (setq *notifications* nil))
  (as:delay
      (eval `(lambda () (notification-handler ,connection)))
    :time 1))

(defun start ()
  (let ((connection (car (postmodern:select-dao 'connection "true LIMIT 1"))))
    (bt:make-thread (lambda ()
                      (let ((postmodern:*database*
                              (postmodern:connect "kawoosh" "kawoosh" "kawoosh" "localhost")))
                        (postmodern:execute "SET TIMEZONE='UTC'")
                        (connection-listen connection))
                      :name "connection-listen"))
    (notification-handler connection)
    (connection-run connection)))

(cl-async:start-event-loop #'start)
