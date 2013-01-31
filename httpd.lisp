(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        clack
        postmodern
        clack.app.route
        clack.request
        json)
  (:shadowing-import-from
   :kawoosh.dao :server-port)
  (:shadowing-import-from
   :kawoosh.dao :server-name)
  (:export start
           app))

(in-package :kawoosh.httpd)

(defgeneric rpc-send (connection command &rest args))

(defmethod rpc-send ((connection connection) command &rest args)
  (apply 'rpc-send (connection-id connection) command args))

(defmethod rpc-send ((connection-id integer) command &rest args)
  (execute (format nil "NOTIFY connection_~a, '~a ~{~@[~a~^ ~]~}'"
                   connection-id
                   (symbol-name command) args)))

(defmacro with-parameters (env keys &rest body)
  `(destructuring-bind (&key ,@keys)
       (getf ,env :route.parameters)
     ,@body))

(defconstant *limit-default* 100)

(defun http-reply (code data)
  `(,code
    (:content-type "application/json")
    (,(encode-json-to-string data))))

(defun success-ok (data)
  (http-reply 200 data))

(defun success-accepted (message)
  (http-reply 202
              `((status . "OK")
                (message . ,message))))

(defun error-not-found (message)
  (http-reply 404
              `((status . "Not Found")
                (message . ,message))))

;; TODO Limit to admin
;; TODO paginate?
(defun user-list (env)
  (success-ok (select-dao 'user)))

(defun user-get (env)
  (with-parameters env (name)
    (let ((user (car (select-dao 'user (:= 'name name)))))
      (if user
          (success-ok user)
          (error-not-found "No such user")))))

;; TODO paginate?
(defun server-list (env)
  (success-ok (select-dao 'server)))

(defun server-get (env)
  (with-parameters env (name)
    (let ((server (car (select-dao 'server (:= 'name name)))))
      (if server
          (success-ok server)
          (error-not-found "No such server")))))

;; TODO paginate?
(defun connection-list (env)
  (with-parameters env (username)
    (success-ok (select-dao 'connection (:= 'username username)))))

(defun connection-get (env)
  (with-parameters env (username server)
    (let ((connection (car (select-dao 'connection (:and (:= 'username username)
                                                         (:= 'server server))))))
      (if connection
          (success-ok connection)
          (error-not-found "No such connection")))))

;; TODO paginate?
(defun channel-list (env)
  (with-parameters env (username server)
    (success-ok (select-dao 'channel (:= 'connection
                                        (:select 'id :from 'connection
                                         :where (:and (:= 'username username)
                                                      (:= 'server server))))))))

;; XXX check content-type?
(defun channel-join (env)
  (with-parameters env (username server channel)
    ;; XXX retrieve only id from connection
    (let ((connection (car (select-dao 'connection
                                       (:and (:= 'username username)
                                             (:= 'server server))))))
      (if connection
          (progn
            (rpc-send connection 'join channel
                      (cdr (assoc :password (decode-json (getf env :raw-body)))))
            (success-accepted (format nil "Joining channel ~a" channel)))
          (error-not-found "No such connection")))))

;; XXX check content-type?
(defun channel-part (env)
  (with-parameters env (username server channel)
    ;; XXX retrieve only id from connection
    (let ((channel-dao (car (select-dao 'channel (:and (:= 'connection
                                                           (:select 'id :from 'connection
                                                            :where (:and (:= 'username username)
                                                                         (:= 'server server))))
                                                       (:= 'name channel))))))
      (if channel-dao
          (progn
            (rpc-send (channel-connection channel-dao) 'part channel
                      (cdr (assoc :reason (decode-json (getf env :raw-body)))))
            (success-accepted (format nil "Parting channel ~a" channel)))
          (error-not-found "No such connection or channel not joined")))))

;; TODO paginate?
(defun channel-get (env)
  (with-parameters env (username server channel)
    (let ((channel (car (select-dao 'channel (:and (:= 'connection
                                                       (:select 'id :from 'connection
                                                        :where (:and (:= 'username username)
                                                                     (:= 'server server))))
                                                   (:= 'name channel))))))
      (if channel
          (success-ok channel)
          (error-not-found "No such connection or channel")))))

;; TODO paginate?
;; TODO ?from=<timestamp>
(defun channel-get-events (env)
  (with-parameters env (username server channel)
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
          (error-not-found "No such connection or channel")))))

(defroutes app
  (GET "/user" #'user-list)
  (GET "/user/:name" #'user-get)

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

(defun start ()
  (clackup #'app))
