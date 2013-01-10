(defpackage kawoosh.httpd
  (:use cl
        kawoosh-dao
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

(defun index (env)
  '(200
    (:content-type "text/plain")
    ("Hello world!")))

;; TODO Limit to admin
;; TODO paginate?
(defun user-list (env)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (mapcar (lambda (user)
                                       (slot-makunbound user 'kawoosh-dao:password))
                                     (select-dao 'user))))))

(defun user-get (env)
  (with-parameters env (name)
    (let ((user (car (select-dao 'user (:= 'name name)))))
      (if user
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string (slot-makunbound user 'kawoosh-dao:password))))
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
