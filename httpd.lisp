(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        clack
        flexi-streams
        postmodern
        clack.app.route
        clack.request
        json)
  (:shadow :stop)
  (:shadowing-import-from
   :kawoosh.dao :server-port)
  (:shadowing-import-from
   :kawoosh.dao :server-name)
  (:export start
           stop
           app))

(in-package :kawoosh.httpd)

(defgeneric rpc-send (connection &rest args))

(defmethod rpc-send ((connection connection) &rest args)
  (apply 'rpc-send (connection-id connection) args))

(defmethod rpc-send ((connection-id integer) &rest args)
  (execute (format nil "NOTIFY connection_~a, '~a'"
                   connection-id
                   (with-output-to-string (s) (prin1 args s)))))

(defmacro with-parameters (env keys &rest body)
  `(destructuring-bind (&key ,@keys)
       (getf ,env :route.parameters)
     ,@body))

(defconstant *limit-default* 100)

(defun http-reply (code &optional data)
  `(,code
    (:content-type "application/json")
    ,(when data
       (list (encode-json-to-string data)))))

(defun success-ok (&optional data)
  (http-reply 200 data))

(defun success-accepted (message)
  (http-reply 202
              `((status . "OK")
                (message . ,message))))

(defun error-not-found (message)
  (http-reply 404
              `((status . "Not Found")
                (message . ,message))))

(defmacro defrouted (name keys &rest body)
  `(defun ,name (env)
     (with-parameters env ,keys
       (with-pg-connection
         ,@body))))

;; TODO Limit to admin
;; TODO paginate?
(defrouted user-list ()
  (success-ok (select-dao 'user)))

(defrouted user-get (name)
  (let ((user (car (select-dao 'user (:= 'name name)))))
    (if user
        (success-ok user)
        (error-not-found "No such user"))))

(defrouted user-put (username)
  (let ((user (make-dao 'user :name username)))
    (success-ok user)))

(defrouted user-delete (username)
  (let ((user (get-dao 'user username)))
    (if user
        (progn
          (delete-dao user)
          (success-ok nil))
        (error-not-found "No such user"))))

(defun user-send-events (username stream)
  "Send new events for USERNAME to STREAM."
  (let* ((last-id 0)
         (logs (with-pg-connection
                   (query-dao 'log-entry
                              (:select 'time 'source 'command 'target 'payload
                               :from (dao-table-name (find-class 'log-entry))
                               :where (:and
                                       (:> 'id last-id)
                                       (:in 'connection
                                            (:select 'id :from 'connection
                                             :where (:= 'username username)))))))))
    (dolist (log logs)
      ;; `encode-json' wants to use `write-char' which doesn't exist for a
      ;; `chunga:chunked-io-stream'
      (write-sequence (string-to-octets (encode-json-to-string log)) stream)
      (write-sequence (string-to-octets "\r\n") stream))))

;; TODO ?from=<timestamp>
(defrouted user-get-events (username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events username stream)))
      (error-not-found "No such user.")))

;; TODO paginate?
(defrouted server-list ()
  (success-ok (select-dao 'server)))

(defrouted server-get (name)
  (let ((server (car (select-dao 'server (:= 'name name)))))
    (if server
        (success-ok server)
        (error-not-found "No such server"))))

;; TODO paginate?
(defrouted connection-list (username)
  (success-ok (select-dao 'connection (:= 'username username))))

(defrouted connection-get (username server)
  (let ((connection (car (select-dao 'connection (:and (:= 'username username)
                                                       (:= 'server server))))))
    (if connection
        (success-ok connection)
        (error-not-found "No such connection"))))

;; TODO paginate?
(defrouted channel-list (username server)
  (success-ok (select-dao 'channel (:= 'connection
                                       (:select 'id :from 'connection
                                        :where (:and (:= 'username username)
                                                     (:= 'server server)))))))

;; XXX check content-type?
(defrouted channel-join (username server channel)
  ;; XXX retrieve only id from connection
  (let ((connection (car (select-dao 'connection
                             (:and (:= 'username username)
                                   (:= 'server server))))))
    (if connection
        (progn
          (rpc-send connection 'irc:join channel
                    :password (cdr (assoc :password (decode-json (getf env :raw-body)))))
          (success-accepted (format nil "Joining channel ~a" channel)))
        (error-not-found "No such connection"))))

;; XXX check content-type?
(defrouted channel-part (username server channel)
  ;; XXX retrieve only id from connection
  (let ((channel-dao (car (select-dao 'channel (:and (:= 'connection
                                                         (:select 'id :from 'connection
                                                          :where (:and (:= 'username username)
                                                                       (:= 'server server))))
                                                     (:= 'name channel))))))
    (if channel-dao
        (progn
          (rpc-send (channel-connection channel-dao) 'irc:part channel
                    (cdr (assoc :reason (decode-json (getf env :raw-body)))))
          (success-accepted (format nil "Parting channel ~a" channel)))
        (error-not-found "No such connection or channel not joined"))))

;; TODO paginate?
(defrouted channel-get (username server channel)
  (let ((channel (car (select-dao 'channel (:and (:= 'connection
                                                     (:select 'id :from 'connection
                                                      :where (:and (:= 'username username)
                                                                   (:= 'server server))))
                                                 (:= 'name channel))))))
    (if channel
        (success-ok channel)
        (error-not-found "No such connection or channel"))))

;; TODO paginate?
;; TODO ?from=<timestamp>
(defrouted channel-get-events (username server channel)
  (let ((logs (query-dao 'log-entry
                         (:limit
                          (:select 'time 'source 'command 'target 'payload
                           :from (dao-table-name (find-class 'log-entry))
                           :where (:and (:= 'connection
                                            (:select 'id :from 'connection
                                             :where (:and (:= 'username username)
                                                          (:= 'server server))))
                                        (:= 'target channel)))
                          (or (query-parameter (make-request env) "limit")
                              *limit-default*)))))
    (if logs
        (success-ok logs)
        (error-not-found "No such connection or channel"))))

(defroutes app
  (GET "/user" #'user-list)
  (GET "/user/:name" #'user-get)
  (PUT "/user/:username" #'user-put)
  (DELETE "/user/:username" #'user-delete)
  (GET "/user/:username/events" #'user-get-events)

  (GET "/server" #'server-list)
  (GET "/server/:name" #'server-get)

  (GET "/user/:username/connection" #'connection-list)
  (GET "/user/:username/connection/:server" #'connection-get)
  ;; (GET "/user/:username/connection/:server/events" #'connection-get-events) ; query log
  ;; (PUT "/user/:username/connection/:server" #'connection-put) ; connect (insert into connection)
  ;; (DELETE "/user/:username/connection/:server" #'connection-delete) ; disconnect (delete from connection)

  ;; (GET "/user/:username/connection/:server/user" #'ircuser-list)
  ;; (GET "/user/:username/connection/:server/user/:ircuser" #'ircuser-get) ; whois
  ;; (GET "/user/:username/connection/:server/user/:ircuser/events" #'ircuser-get-events) ; query log
  ;; (POST "/user/:username/connection/:server/user/:ircuser/message" #'ircuser-get-events) ; privmsg

  (GET "/user/:username/connection/:server/channel" #'channel-list)
  (GET "/user/:username/connection/:server/channel/:channel" #'channel-get)
  (PUT "/user/:username/connection/:server/channel/:channel" #'channel-join) ; TODO can also change mode + topic
  (DELETE "/user/:username/connection/:server/channel/:channel" #'channel-part)

  (GET "/user/:username/connection/:server/channel/:channel/events" #'channel-get-events)
  ;; (POST "/user/:username/connection/:server/channel/:channel/message" #'channel-send-message) ; privmsg

  ;; (GET "/user/:username/connection/:server/channel/:channel/users" #'channel-list-users) ; whois list
  ;; (GET "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-get-user) ; retrieve whois
  ;; (PUT "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-put-user) ; change mode
  ;; (DELETE "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-delete-user) ; kick
  )

(defvar *httpd* nil
  "The running httpd handler.")

(defun start ()
  "Start the Kawoosh httpd server."
  (setq *httpd*
        (clackup #'app)))

(defun stop (&optional (handler *httpd*))
  "Stop the Kawoosh httpd server."
  (clack:stop *httpd*))
