(defpackage kawoosh-httpd
  (:use cl
        kawoosh-dao
        clack
        clack.app.route))

(in-package :kawoosh-httpd)

(defun index (env)
  '(200
    (:content-type "text/plain")
    ("Hello world!")))

(defroutes app
 (GET "/" #'index)
 (GET "/user" #'user-list)
 (GET "/user/:user" #'uset-get)
 (GET "/server" #'server-list)
 (GET "/server/:server" #'server-get)
 (GET "/channel" #'channel-list)
 (GET "/channel/:channel" #'channel-get))

(defun start ()
  (clackup #'app))
