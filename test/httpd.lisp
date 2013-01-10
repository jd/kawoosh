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

(plan 6)

(defmacro do-test (url comment &rest body)
  `(test-app
    #'app
    (lambda ()
      (multiple-value-bind (body status headers)
          (http-request ,url)
        ,@body))
    ,comment))

(do-test "http://localhost:4242/user"
  "Testing user listing"
  (is status 200)
  (is (decode-json-body body) '(((:name . "jd"))))
  (is (cdr (assoc :content-type headers)) "application/json"))

(do-test "http://localhost:4242/user/jd"
  "Testing user retrieval"
  (is status 200)
  (is (decode-json-body body) '((:name . "jd")))
  (is (cdr (assoc :content-type headers)) "application/json"))
