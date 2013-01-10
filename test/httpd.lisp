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

(plan 3)

(test-app
 #'app
 (lambda ()
   (multiple-value-bind (body status headers)
       (http-request "http://localhost:4242/user")
     (is status 200)
     (is (decode-json-body body) '(((:name . "jd"))))
     (is (cdr (assoc :content-type headers)) "application/json")))
 "Testing Kawoosh httpd")
