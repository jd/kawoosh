(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        kawoosh.rpc
        clack
        clack.builder
        clack.middleware.auth.basic
        flexi-streams
        postmodern
        clack.request
        json)
  (:import-from clack.util.route
                make-url-rule
                match)
  (:shadow :stop)
  (:shadowing-import-from
   :kawoosh.dao :server-port)
  (:shadowing-import-from
   :kawoosh.dao :server-name)
  (:export start
           stop))

(in-package :kawoosh.httpd)


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

;; TODO simplify, status is already in HTTP response I think
(defun error-not-found (message)
  (http-reply 404
              `((status . "Not Found")
                (message . ,message))))

(defun error-bad-request (message)
  (http-reply 400
              `((status . "Bad Request")
                (message . ,message))))

(defun error-forbidden (&optional message)
  (http-reply 403
              `((status . "Forbidden")
                (message . ,message))))

(defvar *routes* nil
  "List of routes built into Kawoosh httpd.")

(defmacro defrouted (name keys method url acl &body body)
  `(progn
     (defun ,name (env)
       (with-parameters env ,keys
         (if (user-has-access-p (getf env :remote-user)
                                ',(car acl)
                                ,(cadr acl))
             (with-pg-connection
               ,@body)
             (error-forbidden))))
     (push (cons (make-url-rule ,url :method ',method) #',name)
           *routes*)))

;; TODO paginate?
(defrouted user-list ()
    GET "/user"
    (admin)
  (success-ok (select-dao 'user)))

(defrouted user-get (username)
    GET "/user/:username"
    (user username)
  (let ((user (car (select-dao 'user (:= 'name username)))))
    (if user
        (success-ok user)
        (error-not-found "No such user"))))

(defrouted user-put (username)
    PUT "/user/:username"
    (user username)
  (let* ((body (decode-json (getf env :raw-body)))
         (user (make-dao 'user
                         :name username
                         :password (cdr (assoc :password body)))))
    (success-ok user)))

(defrouted user-delete (username)
    DELETE "/user/:username"
    (admin)
  (let ((user (get-dao 'user username)))
    (if user
        (progn
          (delete-dao user)
          (success-ok nil))
        (error-not-found "No such user"))))

;; FIXME Add a `limit' parameter
;; FIXME Add a `from-timestamp' parameter
;; FIXME Add a `streaming' parameter
(defun user-send-events (class username server stream)
  "Send new events of type CLASS for USERNAME and SERVER to STREAM.
If SERVER is nil, don't filter out on servers."
  (let* ((last-id 0)
         (logs (with-pg-connection
                   (select-dao class
                       (:raw (sql-compile
                              `(:and
                                (:> 'id ,last-id)
                                (:in 'connection
                                     (:select 'id :from 'connection
                                      :where (:and (:= 'username ,username)
                                                   ,(if server
                                                        `(:= server ,server)
                                                        t)))))))))))
    (dolist (log logs)
      ;; `encode-json' wants to use `write-char' which doesn't exist for a
      ;; `chunga:chunked-io-stream'
      (write-sequence (string-to-octets (encode-json-to-string log)) stream)
      ;; \r\n
      (write-sequence #(13 10) stream))))

(defrouted user-get-event (username event-id)
    GET "/user/:username/event/:event-id"
    (user username)
  (let ((event (get-log-entry-for-user username event-id)))
    (if event
        (success-ok event)
        (error-not-found "No such event."))))

;; TODO ?from=<timestamp>
(defrouted user-list-event (username)
    GET "/user/:username/event"
    (user username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events 'log-entry username nil stream)))
      (error-not-found "No such user.")))

;; TODO ?from=<timestamp>
(defrouted user-connection-list-event (username server)
    GET "/user/:username/connection/:server/event"
    (user username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events 'log-entry username server stream)))
      (error-not-found "No such user.")))


;; TODO ?from=<timestamp>
(defrouted user-list-reply (username)
    GET "/user/:username/reply"
    (user username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events 'log-reply username nil stream)))
      (error-not-found "No such user.")))

;; TODO ?from=<timestamp>
(defrouted user-list-command (username)
    GET "/user/:username/command"
    (user username)
  (if (get-dao 'user username)
      `(200
        (:content-type "application/json"
         :transfer-encoding "chunked")
        ,(lambda (stream) (user-send-events 'log-command username nil stream)))
      (error-not-found "No such user.")))

;; TODO paginate?
(defrouted server-list ()
    GET "/server"
    (any)
  (success-ok (select-dao 'server)))

(defrouted server-get (name)
    GET "/server/:name"
    (any)
  (let ((server (car (select-dao 'server (:= 'name name)))))
    (if server
        (success-ok server)
        (error-not-found "No such server"))))

(defrouted server-put (name)
    PUT "/server/:name"
    (admin)
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
(defrouted connection-list (username)
    GET "/user/:username/connection"
    (user username)
  (success-ok (select-dao 'connection (:= 'username username))))

(defrouted connection-get (username server)
    GET "/user/:username/connection/:server"
    (user username)
  (let ((connection (car (select-dao 'connection (:and (:= 'username username)
                                                       (:= 'server server))))))
    (if connection
        (success-ok connection)
        (error-not-found "No such connection"))))

(defrouted connection-put (username server)
    PUT "/user/:username/connection/:server"
    (user username)
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
(defrouted channel-list (username server)
     GET "/user/:username/connection/:server/channel"
    (user username)
  (success-ok (select-dao 'channel (:= 'connection
                                       (:select 'id :from 'connection
                                        :where (:and (:= 'username username)
                                                     (:= 'server server)))))))

;; XXX check content-type?
;; TODO can also change mode + topic
(defrouted channel-join (username server channel)
     PUT "/user/:username/connection/:server/channel/:channel"
    (user username)
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
    DELETE "/user/:username/connection/:server/channel/:channel"
    (user username)
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
    GET "/user/:username/connection/:server/channel/:channel"
    (user username)
  (let ((channel (car (select-dao 'channel (:and (:= 'connection
                                                     (:select 'id :from 'connection
                                                      :where (:and (:= 'username username)
                                                                   (:= 'server server))))
                                                 (:= 'name channel))))))
    (if channel
        (success-ok channel)
        (error-not-found "No such connection or channel"))))

(defrouted channel-user-post-message (username server channel)
    POST "/user/:username/connection/:server/channel/:channel/message"
    (user username)
  (let ((connection (car (select-dao 'connection
                             (:and (:= 'username username)
                                   (:= 'server server))))))
    (if connection
        (progn
          (rpc-send connection 'irc:privmsg channel (read-line (getf env :raw-body) ""))
          (success-accepted (format nil "Sending message to channel ~a" channel)))
        (error-not-found "No such connection"))))

;; TODO paginate?
;; TODO ?from=<timestamp>
(defrouted channel-list-event (username server channel)
    GET "/user/:username/connection/:server/channel/:channel/event"
    (user username)
  ;; FIXME use `user-send-events'!
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
;; (GET "/user/:username/connection/:server/event" #'connection-list-event) ; query log
;; (DELETE "/user/:username/connection/:server" #'connection-delete) ; disconnect (delete from connection)

;; (GET "/user/:username/connection/:server/user" #'ircuser-list)
;; (GET "/user/:username/connection/:server/user/:ircuser" #'ircuser-get) ; whois
;; (GET "/user/:username/connection/:server/user/:ircuser/event" #'ircuser-get-events) ; query log
;; (POST "/user/:username/connection/:server/user/:ircuser/event" #'ircuser-put-event) ; privmsg

;; (GET "/user/:username/connection/:server/channel/:channel/users" #'channel-list-users) ; whois list
;; (GET "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-get-user) ; retrieve whois
;; (PUT "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-put-user) ; change mode
;; (DELETE "/user/:username/connection/:server/channel/:channel/users/:ircuser" #'channel-delete-user) ; kick

(defun application (env)
  "Define the Clack application for Kawoosh."
  (let ((request-method (getf env :request-method))
        (request-path (getf env :path-info)))
    (loop for (url-rule . handler) in *routes*
          do (multiple-value-bind (matched params)
                 (match url-rule request-method request-path)
               (when matched
                 (return (call handler (append env (list :route.parameters params)))))))))

(defvar *httpd* nil
  "The running httpd handler.")

(defun start (&key (port 5000) (debug nil))
  "Start the Kawoosh httpd server."
  (setq *httpd*
        (clackup
         (builder
          (<clack-middleware-auth-basic>
           :realm "Kawoosh API"
           :authenticator (lambda (user password)
                                (let ((dao-user
                                        (with-pg-connection
                                            (get-dao 'user user))))
                                  (when (and dao-user
                                             (string= (user-password dao-user)
                                                      password))
                                    (values t dao-user)))))
          #'application)
         :port port
         :debug debug)))

(defun stop (&optional (handler *httpd*))
  "Stop the Kawoosh httpd server."
  (clack:stop *httpd*))
