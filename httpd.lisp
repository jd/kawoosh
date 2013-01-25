(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        clack
        postmodern
        clack.app.route
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

;; TODO Limit to admin
;; TODO paginate?
(defun user-list (env)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (select-dao 'user)))))

(defun user-get (env)
  (with-parameters env (name)
    (let ((user (car (select-dao 'user (:= 'name name)))))
      (if user
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string user)))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string '((status . "Not Found")
                                       (message . "No such user")))))))))
;; TODO paginate?
(defun server-list (env)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (select-dao 'server)))))

(defun server-get (env)
  (with-parameters env (name)
    (let ((server (car (select-dao 'server (:= 'name name)))))
      (if server
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string server)))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string '((status . "Not Found")
                                       (message . "No such server")))))))))

;; TODO paginate?
(defun connection-list (env)
  (with-parameters env (username)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (select-dao 'connection (:= 'username username)))))))

(defun connection-get (env)
  (with-parameters env (username server)
    (let ((connection (car (select-dao 'connection (:and (:= 'username username)
                                                         (:= 'server server))))))
      (if connection
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string connection)))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string '((status . "Not Found")
                                       (message . "No such connection")))))))))

;; TODO paginate?
(defun channel-list (env)
  (with-parameters env (username server)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (select-dao 'channel (:= 'connection
                                                      (:select 'id :from 'connection
                                                       :where (:and (:= 'username username)
                                                                    (:= 'server server))))))))))

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
            `(202
              (:content-type "application/json")
              (,(encode-json-to-string `((status . "OK")
                                         (message . ,(format nil "Joining channel ~a" channel)))))))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string '((status . "Not Found")
                                       (message . "No such connection")))))))))

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
            `(202
              (:content-type "application/json")
              (,(encode-json-to-string `((status . "OK")
                                         (message . ,(format nil "Parting channel ~a" channel)))))))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string `((status . "Not Found")
                                       (message . ,(format nil "Channel ~a not joined" channel))))))))))

;; TODO paginate?
(defun channel-get (env)
  (with-parameters env (username server channel)
    (let ((channel (car (select-dao 'channel (:and (:= 'connection
                                                       (:select 'id :from 'connection
                                                        :where (:and (:= 'username username)
                                                                     (:= 'server server))))
                                                   (:= 'name channel))))))
      (if channel
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string channel)))
          `(404
            (:content-type "application/json")
            (,(encode-json-to-string '((status . "Not Found")
                                       (message . "No such connection")))))))))

(defroutes app
  (GET "/user" #'user-list)
  (GET "/user/:name" #'user-get)
  (GET "/server" #'server-list)
  (GET "/server/:name" #'server-get)
  (GET "/user/:username/connection" #'connection-list)
  (GET "/user/:username/connection/:server" #'connection-get)
  (GET "/user/:username/connection/:server/channel" #'channel-list)
  (PUT "/user/:username/connection/:server/channel/:channel" #'channel-join)
  (DELETE "/user/:username/connection/:server/channel/:channel" #'channel-part)
  (GET "/user/:username/connection/:server/channel/:channel" #'channel-get))

(defun start ()
  (clackup #'app))
