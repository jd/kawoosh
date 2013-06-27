(defpackage kawoosh.test.httpd
  (:use cl
        cl-json
        clack.test
        drakma
        kawoosh.httpd
        kawoosh.test
        fiveam))

(in-package :kawoosh.test.httpd)

(def-suite kawoosh.test.httpd
  :in kawoosh.test
  :description "Kawoosh HTTPD tests")

(in-suite kawoosh.test.httpd)

;; TODO move to util
(defun decode-json-body (body)
  (decode-json-from-string (flex:octets-to-string body)))

(defmacro with-request (req &rest body)
  `(multiple-value-bind (body status headers)
       ,(if (listp req)
            `(http-request ,(cadr req)
                           :method ,(car req)
                           :content ,(caddr req))
            `(http-request ,req))
     (declare (special body status headers))
     ,@body))

(defmacro is-equal (a b &rest reason-args)
  `(is (equal ,a ,b) ,@reason-args))

(defvar channel-keys
  '(:name :password :modes :names :topic
    :topic--who :topic--time :creation--time))

(def-fixture database ()
  (kawoosh.dao:drop-tables)
  (kawoosh.dao:create-tables)
  (&body)
  (kawoosh.dao:drop-tables))

(test
 httpd-user
 (with-fixture database ()
   ;; TODO move at the end of this test when the data will be empty on startup
   (with-request (:delete "http://localhost:4242/user/jd")
     (is (equal status 200) "Status code 200")
     (is (equal body nil))
     (is (equal (cdr (assoc :content-type headers)) "application/json")))
   (with-request "http://localhost:4242/user"
     (is (equal status 200) "Status code 200")
     (is (equal body nil))
     (is (equal (cdr (assoc :content-type headers)) "application/json")))
   (with-request (:put "http://localhost:4242/user/jd")
     (is (equal status 200) "Status code 200")
     (is (equal (decode-json-body body) '((:name . "jd"))))
     (is (equal (cdr (assoc :content-type headers)) "application/json")))
   (with-request "http://localhost:4242/user"
     (is (equal status 200) "Status code 200")
     (is (equal (decode-json-body body) '(((:name . "jd")))))
     (is (equal (cdr (assoc :content-type headers)) "application/json")))
   (with-request
     "http://localhost:4242/user/foobar"
     (is-equal status 404 "Status code 404")
     (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such user")))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))))

(test
 httpd-user-events-retrieval
 (with-fixture database ()
   (with-request "http://localhost:4242/user/jd/events"
     (is (equal status 200))
     (let ((event (decode-json-body body)))
       (is (equal 5 (length event)))
       (is (equal "PRIVMSG" (cdr (assoc :command event))))
       (is (equal "buddyboy" (cdr (assoc :source event)))))
     (is (equal (cdr (assoc :content-type headers)) "application/json") "Content-type"))))

(test
 httpd-nosuchuser-events-retrieval
 (with-fixture database ()
   (with-request "http://localhost:4242/user/nosuchuser/events"
     (is (equal status 404) "Status code")
     (is (equal (cdr (assoc :content-type headers)) "application/json") "Content-type"))))

(test
 httpd-server
 (with-fixture database ()
   (with-request "http://localhost:4242/server"
     (is-equal status 200 "Status code 200")
     (is-equal (decode-json-body body) '(((:name . "Naquadah")
                                          (:address . "irc.naquadah.org")
                                          (:port . 6667)
                                          (:ssl . t))))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))
   (with-request
     "http://localhost:4242/server/Naquadah"
     (is-equal status 200 "Status code 200")
     (is-equal (decode-json-body body) '((:name . "Naquadah")
                                         (:address . "irc.naquadah.org")
                                         (:port . 6667)
                                         (:ssl . t)))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))
   (with-request
     "http://localhost:4242/server/foobar"
     (is-equal status 404 "Status code 404")
     (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such server")))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))))

(test
 httpd-user-connection-list
 (with-fixture database ()
   (with-request
     "http://localhost:4242/user/jd/connection"
     (is (equal status 200) "Status code 200")
     (let* ((data (decode-json-body (symbol-value 'body)))
            (s (first data)))
       (is-equal (length data) 1 "Number of connection")
       (is-equal (set-exclusive-or (mapcar 'car s)
                                   '(:server :username :nickname :current--nickname
                                     :realname :connected :motd :network-connection))
                 nil
                 "Connection keys")
       (is-equal (cdr (assoc :server s)) "Naquadah" "Server name")
       (is-equal (cdr (assoc :realname s)) "Julien Danjou" "Realname")
       (is-equal (cdr (assoc :nickname s)) "jd" "Nickname")
       (is-equal (cdr (assoc :username s)) "jd" "Username"))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah"
     (is-equal status 200 "Status code 200")
     (let* ((s (decode-json-body (symbol-value 'body))))
       (is-equal (set-exclusive-or (mapcar 'car s)
                                   '(:server :username :nickname :current--nickname
                                     :realname :connected :motd :network-connection))
                 nil
                 "Connection keys")
       (is-equal (cdr (assoc :server s)) "Naquadah" "Server name")
       (is-equal (cdr (assoc :realname s)) "Julien Danjou" "Realname")
       (is-equal (cdr (assoc :nickname s)) "jd" "Nickname")
       (is-equal (cdr (assoc :username s)) "jd" "Username"))
     (is-equal (cdr (assoc :content-type headers)) "application/json"))))

(test
 http-user-channel
 (with-fixture database ()
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah/channel"
     (is-equal status 200 "Status code 200")
     (let* ((data (decode-json-body (symbol-value 'body)))
            (c (car (decode-json-body (symbol-value 'body)))))
       (is-equal (length data) 2 "Number of channels")
       (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Channel keys")
       (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type is JSON"))
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
     (is-equal status 200 "Status code 200")
     (let ((c (decode-json-body (symbol-value 'body))))
       (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Connection keys")
       (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))
   (with-request
       (:put "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
     (is-equal status 202 "Status code")
     (is-equal (decode-json-body (symbol-value 'body))
               '((:status . "OK") (:message . "Joining channel #test"))
               "Message")
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))
   (with-request
       (:put "http://localhost:4242/user/jd/connection/NoConnection/channel/%23test" "{}")
     (is-equal status 404 "Status code")
     (is-equal (decode-json-body (symbol-value 'body))
               '((:status . "Not Found") (:message . "No such connection"))
               "Error message")
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))
   (with-request
       (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
     (is-equal status 202 "Status code")
     (is-equal (decode-json-body (symbol-value 'body))
               '((:status . "OK") (:message . "Parting channel #test"))
               "Message")
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))
   (with-request
       (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/foobar" "{}")
     (is-equal status 404 "Status code")
     (is-equal (decode-json-body (symbol-value 'body))
               '((:status . "Not Found")
                 (:message . "No such connection or channel not joined"))
               "Error message")
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test/events"
     (is-equal status 200 "Status code")
     (let* ((s (decode-json-body body))
            (event (nth 0 s)))
       (is-equal (cdr (assoc :command event)) "PRIVMSG" "Command")
       (is-equal (cdr (assoc :source event)) "buddyboy" "Command"))
     (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))))

(test start-stop "Kawoosh httpd start and stop"
      (start)
      (stop)
      (stop (start)))
