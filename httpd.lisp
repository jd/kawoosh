(defpackage kawoosh-httpd
  (:use cl
        kawoosh-dao
        clack
        postmodern
        clack.app.route
        json)
  (:export start))

(in-package :kawoosh-httpd)

(defmacro with-parameters (env keys &rest body)
  `(destructuring-bind (&key ,@keys)
       (getf ,env :route.parameters)
     ,@body))

(defun index (env)
  '(200
    (:content-type "text/plain")
    ("Hello world!")))

(defun user-get (env)
  (with-parameters env (name)
    (let ((user (car (select-dao 'user (:= 'name name)))))
      ;; XXX DO NOT RETURN PASSWORD DAMN IT
      ;; TODO return 404 on not found
      `(200
        (:content-type "text/plain")
        (,(encode-json-to-string user))))))

(defroutes app
 (GET "/" #'index)
 (GET "/user" #'user-list)
 (GET "/user/:name" #'user-get)
 (GET "/server" #'server-list)
 (GET "/server/:name" #'server-get)
 (GET "/channel" #'channel-list)
 (GET "/channel/:name" #'channel-get))

(defun start ()
  (clackup #'app))
