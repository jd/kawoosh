(defpackage kawoosh.test.httpd
  (:use cl
        cl-json
        clack.test
        drakma
        kawoosh.httpd
        fiveam))

(in-package :kawoosh.test.httpd)

(def-suite kawoosh.test.httpd
  :description "Kawoosh HTTPD tests")

(in-suite kawoosh.test.httpd)

;; TODO move to util
(defun decode-json-body (body)
  (decode-json-from-string (flex:octets-to-string body)))

(defmacro do-test (name req desc &rest body)
  `(test ,name
         ,desc
         (test-app
          #'app
          (lambda ()
            (multiple-value-bind (body status headers)
                ,(if (listp req)
                     `(http-request ,(cadr req)
                                    :method ,(car req)
                                    :content ,(caddr req))
                     `(http-request ,req))
              (declare (special body status headers))
              ,@body)))))

(defmacro is-equal (a b &rest reason-args)
  `(is (equal ,a ,b) ,@reason-args))

(do-test user-listing
  "http://localhost:4242/user"
  "Testing user listing"
  (is (equal status 200) "Status code 200")
  (is (equal (decode-json-body body) '(((:name . "jd")))))
  (is (equal (cdr (assoc :content-type headers)) "application/json")))

(do-test user-retrieval
  "http://localhost:4242/user/jd"
  "Testing user retrieval"
  (is-equal status 200 "Status code 200")
  (is-equal (decode-json-body body) '((:name . "jd")))
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(do-test user-event-retrieval
  "http://localhost:4242/user/jd/events"
  "Message event retrieval"
  (is-equal status 200 "Status code")
  (let* ((s (decode-json-body body))
         (event (nth 0 s)))
    (is-equal (length s) 1)
    (is-equal (cdr (assoc :command event)) "PRIVMSG" "Command")
    (is-equal (cdr (assoc :source event)) "buddyboy" "Command"))
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))

(do-test user-non-existent-retrieval
  "http://localhost:4242/user/foobar"
  "Testing non-existent user retrieval"
  (is-equal status 404 "Status code 404")
  (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such user")))
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(do-test server-listing "http://localhost:4242/server"
  "Testing server listing"
  (is-equal status 200 "Status code 200")
  (is-equal (decode-json-body body) '(((:name . "Naquadah")
                                 (:address . "irc.naquadah.org")
                                 (:port . 6667)
                                 (:ssl . t))))
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(do-test server-retrieval
  "http://localhost:4242/server/Naquadah"
  "Testing server retrieval"
  (is-equal status 200 "Status code 200")
  (is-equal (decode-json-body body) '((:name . "Naquadah")
                                (:address . "irc.naquadah.org")
                                (:port . 6667)
                                (:ssl . t)))
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(do-test server-non-existent-retrieval
  "http://localhost:4242/server/foobar"
  "Testing non-existent server retrieval"
  (is-equal status 404 "Status code 404")
  (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such server")))
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(do-test connection-listing
  "http://localhost:4242/user/jd/connection"
  "Testing connection listing"
  (is-equal status 200 "Status code 200")
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

(do-test connection-retrieval
  "http://localhost:4242/user/jd/connection/Naquadah"
  "Testing connection retrieval"
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
  (is-equal (cdr (assoc :content-type headers)) "application/json"))

(defvar channel-keys
  '(:name :password :modes :names :topic
    :topic--who :topic--time :creation--time))

(do-test channel-listing
  "http://localhost:4242/user/jd/connection/Naquadah/channel"
  "Testing channel listing"
  (is-equal status 200 "Status code 200")
  (let* ((data (decode-json-body (symbol-value 'body)))
         (c (car (decode-json-body (symbol-value 'body)))))
    (is-equal (length data) 2 "Number of channels")
    (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Channel keys")
    (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type is JSON"))

(do-test channel-retrieval
  "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
  "Testing channel retrieval"
  (is-equal status 200 "Status code 200")
  (let ((c (decode-json-body (symbol-value 'body))))
    (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Connection keys")
    (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))

(do-test channel-creation
  (:put "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
  "Testing channel creation"
  (is-equal status 202 "Status code")
  (is-equal (decode-json-body (symbol-value 'body)) '((:status . "OK") (:message . "Joining channel #test"))
      "Message")
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))

(do-test channel-creation-non-existent-connection
  (:put "http://localhost:4242/user/jd/connection/NoConnection/channel/%23test" "{}")
  "Testing channel creation on non existent connection"
  (is-equal status 404 "Status code")
  (is-equal (decode-json-body (symbol-value 'body)) '((:status . "Not Found") (:message . "No such connection"))
      "Error message")
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))

(do-test channel-deletion
  (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
  "Testing channel deletion"
  (is-equal status 202 "Status code")
  (is-equal (decode-json-body (symbol-value 'body)) '((:status . "OK") (:message . "Parting channel #test"))
      "Message")
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))

(do-test channel-non-existent-deletion
  (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/foobar" "{}")
  "Testing non existent channel deletion"
  (is-equal status 404 "Status code")
  (is-equal (decode-json-body (symbol-value 'body)) '((:status . "Not Found")
                                                (:message . "No such connection or channel not joined"))
      "Error message")
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))

(do-test message-event-retrieval
  "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test/events"
  "Message event retrieval"
  (is-equal status 200 "Status code")
  (let* ((s (decode-json-body body))
         (event (nth 0 s)))
    (is-equal (length s) 1)
    (is-equal (cdr (assoc :command event)) "PRIVMSG" "Command")
    (is-equal (cdr (assoc :source event)) "buddyboy" "Command"))
  (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type"))
