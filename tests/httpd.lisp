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

(defmacro do-test (req &rest body)
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

(test
  (kawoosh-httpd-user
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test "http://localhost:4242/user"
    (is (equal status 200) "Status code 200")
    (is (equal (decode-json-body body) '(((:name . "jd")))))
    (is (equal (cdr (assoc :content-type headers)) "application/json"))))

(test
  (kawoosh-httpd-user-retrieval
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test "http://localhost:4242/user/jd"
    (is-equal status 200 "Status code 200")
    (is-equal (decode-json-body body) '((:name . "jd")))
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-user-events-retrieval
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test "http://localhost:4242/user/jd/events"
    (is-equal status 200 "Status code")
    (let ((event (decode-json-body body)))
      (is-equal 5 (length event))
      (is-equal "PRIVMSG" (cdr (assoc :command event)) "Command")
      (is-equal "buddyboy" (cdr (assoc :source event)) "Source"))
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type")))

(test
  (kawoosh-httpd-nosuchuser-events-retrieval
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test "http://localhost:4242/user/nosuchuser/events"
    (is-equal status 404 "Status code")
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type")))

(test
  (kawoosh-httpd-nosuchuser-retrieval
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/user/foobar"
    (is-equal status 404 "Status code 404")
    (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such user")))
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-server-list
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test "http://localhost:4242/server"
    (is-equal status 200 "Status code 200")
    (is-equal (decode-json-body body) '(((:name . "Naquadah")
                                         (:address . "irc.naquadah.org")
                                         (:port . 6667)
                                         (:ssl . t))))
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-server-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/server/Naquadah"
    (is-equal status 200 "Status code 200")
    (is-equal (decode-json-body body) '((:name . "Naquadah")
                                        (:address . "irc.naquadah.org")
                                        (:port . 6667)
                                        (:ssl . t)))
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-nosuchserver-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/server/foobar"
    (is-equal status 404 "Status code 404")
    (is-equal (decode-json-body body) '((:status . "Not Found") (:message . "No such server")))
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-user-connection-list
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/user/jd/connection"
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
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-user-connection-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
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
    (is-equal (cdr (assoc :content-type headers)) "application/json")))

(test
  (kawoosh-httpd-user-connection-channel-list
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/user/jd/connection/Naquadah/channel"
    (is-equal status 200 "Status code 200")
    (let* ((data (decode-json-body (symbol-value 'body)))
           (c (car (decode-json-body (symbol-value 'body)))))
      (is-equal (length data) 2 "Number of channels")
      (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Channel keys")
      (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type is JSON")))

(test
  (kawoosh-httpd-user-connection-channel-list
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
    (is-equal status 200 "Status code 200")
    (let ((c (decode-json-body (symbol-value 'body))))
      (is-equal (set-exclusive-or (mapcar 'car c) channel-keys) nil "Connection keys")
      (is-equal (cdr (assoc :name c)) "#test" "Channel name"))
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON")))

(test
  (kawoosh-httpd-user-connection-channel-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
      (:put "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
    (is-equal status 202 "Status code")
    (is-equal (decode-json-body (symbol-value 'body))
              '((:status . "OK") (:message . "Joining channel #test"))
              "Message")
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON")))

(test
  (kawoosh-httpd-user-nosuchconnection-channel-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
      (:put "http://localhost:4242/user/jd/connection/NoConnection/channel/%23test" "{}")
    (is-equal status 404 "Status code")
    (is-equal (decode-json-body (symbol-value 'body))
              '((:status . "Not Found") (:message . "No such connection"))
              "Error message")
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content type is JSON")))

(test
  (kawoosh-httpd-user-connection-channel-delete
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
      (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
    (is-equal status 202 "Status code")
    (is-equal (decode-json-body (symbol-value 'body))
              '((:status . "OK") (:message . "Parting channel #test"))
              "Message")
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type")))

(test
  (kawoosh-httpd-user-connection-nosuchchannel-delete
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
      (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/foobar" "{}")
    (is-equal status 404 "Status code")
    (is-equal (decode-json-body (symbol-value 'body))
              '((:status . "Not Found")
                (:message . "No such connection or channel not joined"))
              "Error message")
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type")))

(test
  (kawoosh-httpd-user-connection-channel-events-get
   :depends-on (and . (kawoosh.test.worker:irc-connection)))
  (do-test
    "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test/events"
    (is-equal status 200 "Status code")
    (let* ((s (decode-json-body body))
           (event (nth 0 s)))
      (is-equal (length s) 2)
      (is-equal (cdr (assoc :command event)) "PRIVMSG" "Command")
      (is-equal (cdr (assoc :source event)) "buddyboy" "Command"))
    (is-equal (cdr (assoc :content-type headers)) "application/json" "Content-type")))

(test start-stop "Kawoosh httpd start and stop"
      (start)
      (stop)
      (stop (start)))
