(defpackage kawoosh-httpd
  (:use cl
        kawoosh-dao
        clack
        clack.app.route)
  (:export :start))

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
  (with-parameters env (user)
    `(200
      (:content-type "text/plain")
      (,(format nil "Hey hey you are ~a~%" user)))))

(defroutes app
 (GET "/" #'index)
 (GET "/user" #'user-list)
 (GET "/user/:user" #'user-get)
 (GET "/server" #'server-list)
 (GET "/server/:server" #'server-get)
 (GET "/channel" #'channel-list)
 (GET "/channel/:channel" #'channel-get))

(defun start ()
  (clackup #'app))
