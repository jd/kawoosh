(defpackage kawoosh.worker
  (:use cl
        kawoosh.dao
        kawoosh.util)
  (:export start))

(in-package :kawoosh.worker)

(defun connection-socket-read (connection)
  (loop for message = (irc:read-irc-message connection)
        while message
        do (irc:irc-message-event connection message)))

(defun async-event (ev)
  (format t "Socket event: ~a~%" ev))

(defun connection-connect (server port nickname
                           &key
                             (username nil)
                             (realname nil)
                             (password nil)
                             (ssl nil))
  "Connect to server and return a connection object."
  (let* ((connection (make-instance 'irc:connection
                                    :user username
                                    :password password
                                    :server-name server
                                    :server-port port
                                    :client-stream t))
         ;; Use as:tcp-connect to build our network stream, and build a
         ;; closure calling `connection-socket-read' with our `connection'
         ;; as arguments
         (network-stream
           (funcall (if ssl
                        #'cl-async-ssl:tcp-ssl-connect
                        #'as:tcp-connect)
                    server port
                    (lambda (socket stream)
                      (declare (ignore socket stream))
                      (connection-socket-read connection))
                    #'async-event
                    :stream t)))
    ;; Set the network stream on the connection
    (setf (irc:network-stream connection) network-stream)
    ;; Set the output stream on the connection
    (setf (irc:output-stream connection)
         ;; This is grabbed from cl-irc:make-connection
          (flexi-streams:make-flexi-stream
           network-stream
           :element-type 'character
           :external-format '(:utf8 :eol-style :crlf)))

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

(defun notification-handler (connection)
  (multiple-value-bind (channel payload pid)
      (cl-postgres:wait-for-notification postmodern:*database*)
      ;; XXX Ignore pid == self.pid
    (format t "NOTIFICATION RECEIVED ~a ~a ~a~%" channel payload pid)
    (connection-update-channels connection)))

(defun worker-event-loop ()
  (let ((connection (car (postmodern:select-dao 'connection "true LIMIT 1"))))
    (postmodern:execute (format nil "LISTEN channel_~a"(connection-id connection)))
    (as:fd-add
     (get-socket-fd
      (cl-postgres::connection-socket postmodern:*database*))
     :read-cb (lambda () (notification-handler connection))
     :event-cb #'async-event)
    (connection-run connection)))

(defun start ()
  (cl-async:start-event-loop #'worker-event-loop))
