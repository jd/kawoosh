(defpackage kawoosh.test.httpd
  (:use cl
        cl-json
        clack.test
        drakma
        kawoosh.httpd
        cl-test-more))

(in-package :kawoosh.test.httpd)

;; TODO move to util
(defun decode-json-body (body)
  (decode-json-from-string (flex:octets-to-string body)))

(plan 42)

(defvar *tests* nil
  "Test list.")

(defmacro do-test (url comment &rest body)
  `(push ',(list url comment body) *tests*))

(do-test "http://localhost:4242/user"
  "Testing user listing"
  (is status 200 "Status code 200")
  (is (decode-json-body body) '(((:name . "jd"))))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/user/jd"
  "Testing user retrieval"
  (is status 200 "Status code 200")
  (is (decode-json-body body) '((:name . "jd")))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/user/foobar"
  "Testing non-existent user retrieval"
  (is status 404 "Status code 404")
  (is (decode-json-body body) '((:status . "Not Found") (:message . "No such user")))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/server"
  "Testing server listing"
  (is status 200 "Status code 200")
  (is (decode-json-body body) '(((:name . "Naquadah")
                                 (:address . "irc.naquadah.org")
                                 (:port . 6667)
                                 (:ssl . t))))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/server/Naquadah"
  "Testing server retrieval"
  (is status 200 "Status code 200")
  (is (decode-json-body body) '((:name . "Naquadah")
                                (:address . "irc.naquadah.org")
                                (:port . 6667)
                                (:ssl . t)))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/server/foobar"
  "Testing non-existent server retrieval"
  (is status 404 "Status code 404")
  (is (decode-json-body body) '((:status . "Not Found") (:message . "No such server")))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/user/jd/connection"
  "Testing connection listing"
  (is status 200 "Status code 200")
  (let* ((data (decode-json-body (symbol-value 'body)))
         (s (first data)))
    (is (length data) 1 "Number of connection")
    (is (set-exclusive-or (mapcar 'car s)
                          '(:server :username :nickname :current--nickname
                            :realname :connected :motd :network-connection))
        nil
        "Connection keys")
    (is (cdr (assoc :server s)) "Naquadah" "Server name")
    (is (cdr (assoc :realname s)) "Julien Danjou" "Realname")
    (is (cdr (assoc :nickname s)) "jd" "Nickname")
    (is (cdr (assoc :username s)) "jd" "Username"))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/user/jd/connection/Naquadah"
  "Testing connection retrieval"
  (is status 200 "Status code 200")
  (let* ((s (decode-json-body (symbol-value 'body))))
    (is (set-exclusive-or (mapcar 'car s)
                          '(:server :username :nickname :current--nickname
                            :realname :connected :motd :network-connection))
        nil
        "Connection keys")
    (is (cdr (assoc :server s)) "Naquadah" "Server name")
    (is (cdr (assoc :realname s)) "Julien Danjou" "Realname")
    (is (cdr (assoc :nickname s)) "jd" "Nickname")
    (is (cdr (assoc :username s)) "jd" "Username"))
  (is (cdr (assoc :content-type headers)) "application/json"))

(defvar channel-keys
  '(:name :password :modes :names :topic
    :topic--who :topic--time :creation--time))

(do-test "http://localhost:4242/user/jd/connection/Naquadah/channel"
  "Testing channel listing"
  (is status 200 "Status code 200")
  (let* ((data (decode-json-body (symbol-value 'body)))
         (c (car (decode-json-body (symbol-value 'body)))))
    (is (length data) 2 "Number of channels")
    (is (set-exclusive-or (mapcar 'car c) channel-keys) nil "Channel keys")
    (is (cdr (assoc :name c)) "#test" "Channel name"))
  (is (cdr (assoc :content-type headers)) "application/json" "Content-type is JSON"))

(do-test "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
  "Testing channel retrieval"
  (is status 200 "Status code 200")
  (let ((c (decode-json-body (symbol-value 'body))))
    (is (set-exclusive-or (mapcar 'car c) channel-keys) nil "Connection keys")
    (is (cdr (assoc :name c)) "#test" "Channel name"))
  (is (cdr (assoc :content-type headers)) "application/json" "Content type is JSON"))

;; Really run the tests now
(define-app-test
    httpd
  #'app
  (lambda ()
    (loop for test in (reverse *tests*)
          do (multiple-value-bind (body status headers)
                 (http-request (first test))
               (declare (special body status headers))
               (diag (second test))
               (dolist (test-body (third test))
                 (eval test-body))))))
