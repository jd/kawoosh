(defpackage kawoosh.worker
  (:use cl
        kawoosh.dao)
  (:export start))

(in-package :kawoosh.worker)
;; MAIN

(defvar *stream->connection* (make-hash-table))

(defun connection-socket-read (socket stream)
  (declare (ignore socket))
  (loop with connection = (gethash stream *stream->connection*)
        for message = (irc:read-irc-message connection)
        while message
        do (irc:irc-message-event connection message)))

(defun connection-socket-event (ev)
  (format t "Socket event: ~a~%" ev))

(defun connection-connect (server port nickname
                           &key
                             (username nil)
                             (realname nil)
                             (password nil)
                             (ssl nil))
  "Connect to server and return a connection object."
  (let ((connection (irc:make-connection :connection-type 'irc:connection
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
    (setf (gethash (irc:network-stream connection) *stream->connection*)
          connection)

    (irc:add-default-hooks connection)

    (unless (null password)
      (irc:pass connection password))
    (irc:nick connection nickname)
    (irc:user- connection (or username nickname) 0 (or realname nickname))
    connection))

(defun irc-message-received-timestamp (msg)
  "Return the SQL timestamp for msg."
  (simple-date:universal-time-to-timestamp
   (irc:received-time msg)))

(defun irc-user-serialize (user)
  (format nil "~a!~a@~a"
          (irc:nickname user)
          (irc:username user)
          (irc:hostname user)))

(defun connection-update-channels (connection)
  ;; Check that connection has been set (= connected) otherwise don't do anything.
    (let* ((network-connection (connection-network-connection connection))
           (wanted-channels (postmodern:select-dao 'channel (:= 'connection (connection-id connection))))
           (wanted-channels-name (mapcar #'channel-name wanted-channels)))
      ;; PART channels not in the table anymore
      (loop for channel-name being the hash-keys of (irc:channels network-connection)
            unless (find channel-name wanted-channels-name :test #'string=)
              do (irc:part network-connection channel-name))
      ;; JOIN new channels
      (loop for channel in wanted-channels
            do (irc:join network-connection
                            (channel-name channel)
                            :password (channel-password channel))
               ;; Send an empty mode command to retrieve channel mode and
               ;; eventually creation time
            do (irc:mode network-connection
                            (channel-name channel)))))

(defun connection-add-hook (connection msgclass hook &optional last)
  "Add a server hook for msgclass.
If last is not nil, put the hook in the last run ones."
  (funcall (if last
               #'irc:append-hook
               #'irc:add-hook)
           (connection-network-connection connection)
           msgclass
           (lambda (msg)
             (funcall hook connection msg))))

(defun connection-log-msg (connection msg)
  (postmodern:execute
   "INSERT INTO logs (connection, time, source, command, target, payload) VALUES ($1, $2, $3, $4, $5, $6)"
   (connection-id connection)
   (irc-message-received-timestamp msg)
   (irc:source msg)
   (irc:command msg)
   (car (irc:arguments msg))
   (cadr (irc:arguments msg))))

(defun connection-handle-rpl_welcome (connection msg)
  ;; This is at least needed to store and update current-nickname
  (destructuring-bind (nickname welcome-msg) (irc:arguments msg)
    (declare (ignore welcome-msg))
    (setf (connection-current-nickname connection) nickname))
  (setf (connection-connected-p connection) t)
  (postmodern:update-dao connection)
  (connection-update-channels connection))

(defun connection-handle-rpl_motd (connection msg)
  (destructuring-bind (target text) (irc:arguments msg)
    (declare (ignore target))
    (setf (connection-motd connection)
          (format nil "~a~a~%"
                  (let ((motd (connection-motd connection)))
                    (if (eq motd :null) "" motd))
                  text))))

(defun connection-handle-rpl_endofmotd (connection msg)
  (destructuring-bind (target text) (irc:arguments msg)
    (declare (ignore target text))
    (postmodern:update-dao connection)))

;; XXX unit test me
;; XXX move this into a utility package
(defun list->array (l)
  "Recursive list to array conversion."
  (make-array (length l) :initial-contents l))

(defun channel-update-names (connection channel)
  (let ((users (loop for user being the hash-values of
                                                    (irc:users (irc:find-channel (connection-network-connection connection)
                                                                                       (channel-name channel)))
                     collect (irc-user-serialize user))))
    (setf (channel-names channel)
          (if users (list->array users) :null))
    (postmodern:update-dao channel)))

(defun irc-channel-serialize-mode (channel)
  (loop for (mode raw-value) on (irc:modes channel) by #'cddr
        for value = (irc:get-mode channel mode)
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
        (let ((modes (irc-channel-serialize-mode (irc:find-channel (connection-network-connection connection)
                                                                      (channel-name channel)))))
          (if modes
              (list->array modes)
              :null)))
  (postmodern:update-dao channel))

(defun channel-find (connection channel-name)
  (car (postmodern:select-dao 'channel (:and (:= 'name channel-name)
                                             (:= 'connection (connection-id connection))))))

(defun connection-handle-join (connection msg)
  (unless (irc:self-message-p msg)
    (destructuring-bind (channel-name) (irc:arguments msg)
      (channel-update-names connection (channel-find connection channel-name)))))

(defun connection-handle-part (connection msg)
  (unless (irc:self-message-p msg)
    (destructuring-bind (channel-name &optional text) (irc:arguments msg)
      (declare (ignore text))
      (channel-update-names connection (channel-find connection channel-name)))))

(defun connection-handle-quit (connection msg)
  (declare (ignore msg))
  ;; TODO this can be optimized by only iterating on channels that have the
  ;; user (aka `(irc:source msg)') in the channel.names array.
  (dolist (channel (postmodern:select-dao 'channel (:= 'connection (connection-id connection))))
    (channel-update-names connection channel)))

(defun connection-handle-rpl_endofnames (connection msg)
  (destructuring-bind (nickname channel-name text) (irc:arguments msg)
    (declare (ignore nickname text))
    (let ((channel (channel-find connection channel-name)))
      ;; TODO only one DAO update please
      (channel-update-names connection channel)
      (channel-update-mode connection channel))))

(defun connection-handle-rpl_channelmodeis (connection msg)
  (destructuring-bind (target channel &rest mode-arguments) (irc:arguments msg)
    (declare (ignore target mode-arguments))
    (channel-update-mode connection (channel-find connection channel))))

(defun connection-handle-mode (connection msg)
  (destructuring-bind (target &rest mode-arguments) (irc:arguments msg)
    (declare (ignore mode-arguments))
    (let ((channel (channel-find connection target)))
      (when channel
        (channel-update-mode connection channel)))))

(defun connection-handle-rpl_topic (connection msg)
  (destructuring-bind (target channel-name &optional topic) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic channel) topic)
      (postmodern:update-dao channel))))

(defun connection-handle-topic (connection msg)
  (destructuring-bind (channel-name &optional topic) (irc:arguments msg)
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic channel) topic)
      (setf (channel-topic-who channel) (irc:source msg))
      (setf (channel-topic-time channel) (irc-message-received-timestamp msg))
      (postmodern:update-dao channel))))

;; XXX move to util
(defun unix-time->timestamp (unix-time)
  (simple-date:universal-time-to-timestamp
   (local-time:timestamp-to-universal
    (local-time:unix-to-timestamp unix-time))))

(defun connection-handle-rpl_topicwhotime (connection msg)
  (destructuring-bind (target channel-name who time) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-topic-who channel) who)
      (setf (channel-topic-time channel) (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

(defun connection-handle-rpl_creationtime (connection msg)
  (destructuring-bind (target channel-name time) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (channel-find connection channel-name)))
      (setf (channel-creation-time channel)
            (unix-time->timestamp (parse-integer time)))
      (postmodern:update-dao channel))))

(defun connection-handle-nick (connection msg)
  "Handle nick change."
  (destructuring-bind (new-nick) (irc:arguments msg)
    (setf (connection-current-nickname connection) new-nick))
  (postmodern:update-dao connection))

(defun connection-handle-err_nicknameinuse-message (connection msg)
  "Handle nick already in used error."
  ;; Try to change the nick to "current-nickname + _"
  (destructuring-bind (_ tried-nickname error) (irc:arguments msg)
    (declare (ignore _ error))
    (irc:nick (connection-network-connection connection)
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
                       'irc:irc-err_nicknameinuse-message
                       #'connection-handle-err_nicknameinuse-message)

  ;; These hooks have to be last to be ran after irc internally updated
  ;; its copy of users in connection/channels
  (connection-add-hook connection 'irc:irc-join-message #'connection-handle-join t)
  (connection-add-hook connection 'irc:irc-part-message #'connection-handle-part t)
  (connection-add-hook connection 'irc:irc-quit-message #'connection-handle-quit t)
  (connection-add-hook connection 'irc:irc-rpl_endofnames-message #'connection-handle-rpl_endofnames t)
  (connection-add-hook connection 'irc:irc-rpl_channelmodeis-message #'connection-handle-rpl_channelmodeis t)
  (connection-add-hook connection 'irc:irc-mode-message #'connection-handle-mode t)

  ;; Channel topic handling
  (connection-add-hook connection 'irc:irc-topic-message #'connection-handle-topic)
  (connection-add-hook connection 'irc:irc-rpl_topic-message #'connection-handle-rpl_topic)
  (connection-add-hook connection 'irc:irc-rpl_topicwhotime-message #'connection-handle-rpl_topicwhotime)

  (connection-add-hook connection 'irc:irc-rpl_creationtime-message #'connection-handle-rpl_creationtime)

  (connection-add-hook connection 'irc:irc-rpl_welcome-message #'connection-handle-rpl_welcome)
  (connection-add-hook connection 'irc:irc-rpl_motd-message #'connection-handle-rpl_motd)
  (connection-add-hook connection 'irc:irc-rpl_endofmotd-message #'connection-handle-rpl_endofmotd)
  (connection-add-hook connection 'irc:irc-nick-message #'connection-handle-nick)
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
   (lambda () (notification-handler connection))
   :time 1))

(defun worker-event-loop ()
  (let ((connection (car (postmodern:select-dao 'connection "true LIMIT 1"))))
    ;; (bt:make-thread (lambda ()
    ;;                   (let ((postmodern:*database*
    ;;                           (postmodern:connect "kawoosh" "kawoosh" "kawoosh" "localhost")))
    ;;                     (postmodern:execute "SET TIMEZONE='UTC'")
    ;;                     (connection-listen connection))
    ;;                   :name "connection-listen"))
    (notification-handler connection)
    (connection-run connection)))

(defun start ()
  (cl-async:start-event-loop #'worker-event-loop))
