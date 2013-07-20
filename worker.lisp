(defpackage kawoosh.worker
  (:use bordeaux-threads
        cl
        cl-postgres
        postmodern
        kawoosh.dao
        kawoosh.util)
  (:export start
           pick-connection))

(in-package :kawoosh.worker)

(defun irc-message-received-timestamp (msg)
  "Return the SQL timestamp for msg."
  (simple-date:universal-time-to-timestamp
   (irc:received-time msg)))

(defun irc-user-serialize (user)
  (format nil "~a!~a@~a"
          (irc:nickname user)
          (irc:username user)
          (irc:hostname user)))

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

(defun connection-handle-rpl_welcome (connection msg)
  ;; This is at least needed to store and update current-nickname
  (destructuring-bind (nickname welcome-msg) (irc:arguments msg)
    (declare (ignore welcome-msg))
    (setf (connection-current-nickname connection) nickname))
  (setf (connection-connected-p connection) t)
  (update-dao connection)

  ;; Join channels
  (let ((network-connection (connection-network-connection connection)))
    (loop for channel in (select-dao 'channel (:= 'connection (connection-id connection)))
          do (irc:join network-connection
                       (channel-name channel)
                       :password (channel-password channel))
             ;; Send an empty mode command to retrieve channel mode and
             ;; eventually creation time
          do (irc:mode network-connection
                       (channel-name channel)))))

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
    (update-dao connection)))


(defun channel-update-names (connection channel)
  (let ((users (loop for user being the hash-values of
                                                    (irc:users (irc:find-channel (connection-network-connection connection)
                                                                                       (channel-name channel)))
                     collect (irc-user-serialize user))))
    (setf (channel-names channel)
          (if users (list->array users) :null))
    (update-dao channel)))

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
  (update-dao channel))

(defun connection-handle-join (connection msg)
  (destructuring-bind (channel-name) (irc:arguments msg)
    (if (irc:self-message-p msg)
        (save-dao (make-instance 'channel :connection (connection-id connection) :name channel-name
                                          :joined-at (irc-message-received-timestamp msg)))
        (channel-update-names connection (get-dao 'channel (connection-id connection) channel-name)))))

(defun connection-handle-part (connection msg)
  (destructuring-bind (channel-name &optional text) (irc:arguments msg)
    (declare (ignore text))
    (let ((channel (get-dao 'channel (connection-id connection) channel-name)))
      (if (irc:self-message-p msg)
          (when channel
            (delete-dao channel))
          (channel-update-names connection channel)))))

(defun connection-handle-quit (connection msg)
  (declare (ignore msg))
  ;; TODO this can be optimized by only iterating on channels that have the
  ;; user (aka `(irc:source msg)') in the channel.names array.
  (dolist (channel (select-dao 'channel (:= 'connection (connection-id connection))))
    (channel-update-names connection channel)))

(defun connection-handle-rpl_endofnames (connection msg)
  (destructuring-bind (nickname channel-name text) (irc:arguments msg)
    (declare (ignore nickname text))
    (let ((channel (get-dao 'channel (connection-id connection) channel-name)))
      ;; TODO only one DAO update please
      (channel-update-names connection channel)
      (channel-update-mode connection channel))))

(defun connection-handle-rpl_channelmodeis (connection msg)
  (destructuring-bind (target channel &rest mode-arguments) (irc:arguments msg)
    (declare (ignore target mode-arguments))
    (channel-update-mode connection (get-dao 'channel (connection-id connection) channel))))

(defun connection-handle-mode (connection msg)
  (destructuring-bind (target &rest mode-arguments) (irc:arguments msg)
    (declare (ignore mode-arguments))
    (let ((channel (get-dao 'channel (connection-id connection) target)))
      (when channel
        (channel-update-mode connection channel)))))

(defun connection-handle-rpl_topic (connection msg)
  (destructuring-bind (target channel-name &optional topic) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (get-dao 'channel (connection-id connection) channel-name)))
      (when channel
        (setf (channel-topic channel) topic)
        (update-dao channel)))))

(defun connection-handle-topic (connection msg)
  (destructuring-bind (channel-name &optional topic) (irc:arguments msg)
    (let ((channel (get-dao 'channel (connection-id connection) channel-name)))
      (when channel
        (setf (channel-topic channel) topic)
        (setf (channel-topic-who channel) (irc:source msg))
        (setf (channel-topic-time channel) (irc-message-received-timestamp msg))
        (update-dao channel)))))

(defun unix-time->timestamp (unix-time)
  (simple-date:universal-time-to-timestamp
   (local-time:timestamp-to-universal
    (local-time:unix-to-timestamp unix-time))))

(defun connection-handle-rpl_topicwhotime (connection msg)
  (destructuring-bind (target channel-name who time) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (get-dao 'channel (connection-id connection) channel-name)))
      (when channel
        (setf (channel-topic-who channel) who)
        (setf (channel-topic-time channel) (unix-time->timestamp (parse-integer time)))
        (update-dao channel)))))

(defun connection-handle-rpl_creationtime (connection msg)
  (destructuring-bind (target channel-name time) (irc:arguments msg)
    (declare (ignore target))
    (let ((channel (get-dao (connection-id connection) channel-name)))
      (when channel
        (setf (channel-creation-time channel)
              (unix-time->timestamp (parse-integer time)))
        (update-dao channel)))))

(defun connection-handle-nick (connection msg)
  "Handle nick change."
  (destructuring-bind (new-nick) (irc:arguments msg)
    (setf (connection-current-nickname connection) new-nick))
  (update-dao connection))

(defun connection-handle-err_nicknameinuse-message (connection msg)
  "Handle nick already in used error."
  ;; Try to change the nick to "current-nickname + _"
  (destructuring-bind (_ tried-nickname error) (irc:arguments msg)
    (declare (ignore _ error))
    (irc:nick (connection-network-connection connection)
                 (format nil "~a_" tried-nickname))))

(defclass irc-connection (irc:connection)
  nil)

(defun connection-run (connection)
  "Connect to the connection."
  (let ((server (get-dao 'server (connection-server connection))))
    (setf (connection-network-connection connection)
          (irc:connect :server (format nil "~a" (server-address server))
                       :connection-type 'irc-connection
                       :port (server-port server)
                       :nickname (connection-nickname connection)
                       :connection-security (if (server-ssl-p server) :ssl :none)
                       :realname (connection-realname connection))))

  ;; Update (clean) the connection entry in the database
  (setf (connection-current-nickname connection) :null)
  (setf (connection-connected-p connection) nil)
  (setf (connection-motd connection) :null)
  (update-dao connection)

  (dolist (channel (select-dao 'channel (:= 'connection (connection-id connection))))
    (setf (channel-names channel) :null)
    (setf (channel-modes channel) :null)
    (setf (channel-topic channel) :null)
    (setf (channel-topic-who channel) :null)
    (setf (channel-topic-time channel) :null)
    (setf (channel-creation-time channel) :null)
    (update-dao channel))

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

  (defmethod irc::send-irc-message ((irc-connection (eql (connection-network-connection connection)))
                                    command &rest arguments)
    (call-next-method)
    (execute
     "INSERT INTO command (connection, source, command, target, payload) VALUES ($1, $2, $3, $4, $5)"
     (connection-id connection)
     (let ((user (irc:user irc-connection)))
       (if user
           (irc:nickname user)
           ""))
     (symbol-name command)
     (car arguments)
     (format nil "~{~a~^-~}" (cdr arguments))))

  (defmethod irc:irc-message-event ((irc-connection (eql (connection-network-connection connection)))
                                    (message irc:irc-message))
    (execute
     (format
      nil
      "INSERT INTO ~a (connection, time, source, command, target, payload) VALUES ($1, $2, $3, $4, $5, $6)"
      (if (string= (irc:command message) "ERR" :end1 3)
          "error"
          "reply"))
     (connection-id connection)
     (irc-message-received-timestamp message)
     (irc:source message)
     (irc:command message)
     (car (irc:arguments message))
     (or (cadr (irc:arguments message)) :null))
    (call-next-method))

  ;; Endless loop starts here
  (irc:read-message-loop (connection-network-connection connection)))

(defun rpc-start (connection)
  "Start listening on RPC channel for CONNECTION."
  (with-pg-connection
    (execute (format nil "LISTEN connection_~a" (connection-id connection)))
    (loop while t
          do (multiple-value-bind (channel payload pid)
                 (wait-for-notification *database*)
               (let ((command (read-from-string payload)))
                 (eval (cons (car command)
                             ;; Insert the connection as first argument
                             (cons (connection-network-connection connection)
                                   (cdr command)))))))))

(defun pick-connection ()
  (with-pg-connection
    (car (select-dao 'connection "true LIMIT 1"))))

(defun start (&optional connection)
  (let ((connection (or connection (pick-connection))))
    (make-thread (lambda () (rpc-start connection))
                 :name "Kawoosh worker RPC")
    (with-pg-connection
      (connection-run connection))))
