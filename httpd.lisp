(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        clack
        clack.builder
        clack.middleware.auth.basic
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
  (if data
      (http-reply 200 data)
      (http-reply 204)))

(defun success-accepted (message)
  (http-reply 202
              `((status . "OK")
                (message . ,message))))

(defun error-not-found (message)
  (http-reply 404
              `((status . "Not Found")
                (message . ,message))))

(defun error-bad-request (message)
  (http-reply 400
              `((status . "Bad Request")
                (message . ,message))))

(defvar *routes* nil
  "List of routes built into Kawoosh httpd.")

(defmacro defrouted (method url name keys &body body)
  `(progn
     (defun ,name (env)
       (with-parameters env ,keys
         (with-pg-connection
           ,@body)))
     (push (list ',method ,url #',name) *routes*)))

;; TODO Limit to admin
;; TODO paginate?
(defrouted GET "/user" user-list ()
  (success-ok (select-dao 'user)))

(defrouted GET "/user/:name" user-get (name)
  (let ((user (car (select-dao 'user (:= 'name name)))))
    (if user
        (success-ok user)
        (error-not-found "No such user"))))

(defrouted PUT "/user/:username" user-put (username)
  (let ((user (make-dao 'user :name username)))
    (success-ok user)))

(defrouted DELETE "/user/:username" user-delete (username)
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
(defrouted GET "/user/:username/events" user-get-events (username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events username stream)))
      (error-not-found "No such user.")))

;; TODO paginate?
(defrouted GET "/server" server-list ()
  (success-ok (select-dao 'server)))

(defrouted GET "/server/:name" server-get (name)
  (let ((server (car (select-dao 'server (:= 'name name)))))
    (if server
        (success-ok server)
        (error-not-found "No such server"))))

(defrouted PUT "/server/:name" server-put (name)
  (let* ((body (decode-json (getf env :raw-body)))
         (args (list :name name
                     :ssl (cdr (assoc :ssl body))
                     :address (cdr (assoc :address body))))
         (port (cdr (assoc :port body)))
         (server (apply #'make-instance 'server
                        (if port (append args (list :port port)) args))))
    (handler-case
        (save-dao server)
      ;; TODO more detailed errors
      (error () (error-bad-request "Invalid server details"))
      (:no-error (inserted) (success-ok server)))))

;; TODO paginate?
(defrouted GET "/user/:username/connection" connection-list (username)
  (success-ok (select-dao 'connection (:= 'username username))))

(defrouted GET "/user/:username/connection/:server" connection-get (username server)
  (let ((connection (car (select-dao 'connection (:and (:= 'username username)
                                                       (:= 'server server))))))
    (if connection
        (success-ok connection)
        (error-not-found "No such connection"))))

(defrouted PUT "/user/:username/connection/:server" connection-put (username server)
  (let* ((body (decode-json (getf env :raw-body)))
         (nickname (cdr (assoc :nickname body)))
         (connection (make-instance 'connection :username username
                                                :nickname nickname
                                                :server server)))
    (handler-case
        (save-dao connection)
      (error () (error-bad-request "Invalid connection details"))
      (:no-error (inserted) (success-ok connection)))))

;; TODO paginate?
(defrouted GET "/user/:username/connection/:server/channel" channel-list (username server)
  (success-ok (select-dao 'channel (:= 'connection
                                       (:select 'id :from 'connection
                                        :where (:and (:= 'username username)
                                                     (:= 'server server)))))))

;; XXX check content-type?
;; TODO can also change mode + topic
(defrouted PUT "/user/:username/connection/:server/channel/:channel" channel-join (username server channel)
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
(defrouted DELETE "/user/:username/connection/:server/channel/:channel" channel-part (username server channel)
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
(defrouted GET "/user/:username/connection/:server/channel/:channel" channel-get (username server channel)
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
(defrouted GET "/user/:username/connection/:server/channel/:channel/events" channel-get-events (username server channel)
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


;; TODO likely missing:
;; (GET "/user/:username/connection/:server/events" #'connection-get-events) ; query log
;; (DELETE "/user/:username/connection/:server" #'connection-delete) ; disconnect (delete from connection)

;; (GET "/user/:username/connection/:server/user" #'ircuser-list)
;; (GET "/user/:username/connection/:server/user/:ircuser" #'ircuser-get) ; whois
;; (GET "/user/:username/connection/:server/user/:ircuser/events" #'ircuser-get-events) ; query log
;; (POST "/user/:username/connection/:server/user/:ircuser/message" #'ircuser-get-events) ; privmsg

;; (POST "/user/:username/connection/:server/channel/:channel/message" #'channel-send-message) ; privmsg

;; (GET "/user/:username/connection/:server/channel/:channel/users" #'channel-list-users) ; whois list
;; (GET "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-get-user) ; retrieve whois
;; (PUT "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-put-user) ; change mode
;; (DELETE "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-delete-user) ; kick


(eval (macroexpand `(defroutes app ,@*routes*)))

(defvar *httpd* nil
  "The running httpd handler.")

(defun start (&key (port 5000) (debug nil))
  "Start the Kawoosh httpd server."
  (setq *httpd*
        (clackup
         (builder
          (<clack-middleware-auth-basic>
           :realm "Kawoosh API"
           :authenticator (lambda (user pass)
                            (with-pg-connection
                                (let ((dao-user (get-dao 'user :name user)))
                                  (when (string= pass (user-password dao-user))
                                    (values t dao-user))))))
          #'app)
         :port port
         :debug debug)))

(defun stop (&optional (handler *httpd*))
  "Stop the Kawoosh httpd server."
  (clack:stop *httpd*))
