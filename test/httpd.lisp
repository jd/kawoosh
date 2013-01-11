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

(plan 18)

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


;; Really run the tests now
(test-app
 #'app
 (lambda ()
   (loop for test in (reverse *tests*)
         do (multiple-value-bind (body status headers)
                (http-request (first test))
              (declare (special body status headers))
              (diag (second test))
              (dolist (test-body (third test))
                (eval test-body))))))
