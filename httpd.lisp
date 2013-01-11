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


(defmethod json:encode-json ((o dao-object)
                             &optional (stream json:*json-output*))
  "Write the JSON representation (Object) of the postmodern DAO CLOS object
O to STREAM (or to *JSON-OUTPUT*)."
  (json:with-object (stream)
    (json::map-slots (lambda (key value)
                       (as-object-member (key stream)
                         (json:encode-json (if (eq value :null) nil value) stream)))
                     o)))

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
                                       (slot-makunbound user 'kawoosh.dao:password))
                                     (select-dao 'user))))))

(defun user-get (env)
  (with-parameters env (name)
    (let ((user (car (select-dao 'user (:= 'name name)))))
      (if user
          `(200
            (:content-type "application/json")
            (,(encode-json-to-string (slot-makunbound user 'kawoosh.dao:password))))
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
  (with-parameters env (name)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (mapcar (lambda (c) (slot-makunbound c 'kawoosh.dao:id))
                                     (select-dao 'connection (:= 'username name))))))))


(defroutes app
 (GET "/" #'index)
 (GET "/user" #'user-list)
 (GET "/user/:name" #'user-get)
 (GET "/server" #'server-list)
 (GET "/server/:name" #'server-get)
 (GET "/user/:name/connection" #'connection-list)
 (GET "/user/:name/connection/:server" #'connection-get))

(defun start ()
  (clackup #'app))
