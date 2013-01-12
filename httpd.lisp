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

(defmethod encode-json ((o kawoosh.dao:dao-object)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the postmodern DAO CLOS object
O to STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (json::map-slots (lambda (key value)
                 (as-object-member (key stream)
                   (encode-json (if (eq value :null) nil value) stream)))
               o)))


(defmethod encode-json ((o kawoosh.dao:connection)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the postmodern DAO CLOS object
O to STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (json::map-slots (lambda (key value)
                       (unless (member key '(kawoosh.dao:id))
                         (as-object-member (key stream)
                           (encode-json (if (eq value :null) nil value) stream))))
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
  (with-parameters env (username)
  `(200
    (:content-type "application/json")
    (,(encode-json-to-string (mapcar (lambda (c) (slot-makunbound c 'kawoosh.dao:id))
                                     (select-dao 'connection (:= 'username username))))))))

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


(defroutes app
 (GET "/" #'index)
 (GET "/user" #'user-list)
 (GET "/user/:name" #'user-get)
 (GET "/server" #'server-list)
 (GET "/server/:name" #'server-get)
 (GET "/user/:username/connection" #'connection-list)
 (GET "/user/:username/connection/:server" #'connection-get))

(defun start ()
  (clackup #'app))
