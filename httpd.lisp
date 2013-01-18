(defpackage kawoosh.httpd
  (:use cl
        kawoosh.dao
        clack
        postmodern
        clack.app.route
        json)
  (:export start
           app))

(in-package :kawoosh.httpd)

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

(defroutes app
  (GET "/user" #'user-list)
  (GET "/user/:name" #'user-get)
  (GET "/server" #'server-list)
  (GET "/server/:name" #'server-get)
  (GET "/user/:username/connection" #'connection-list)
  (GET "/user/:username/connection/:server" #'connection-get)
  (GET "/user/:username/connection/:server/channel" #'channel-list))

(defun start ()
  (clackup #'app))
