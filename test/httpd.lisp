(defpackage kawoosh.test.httpd
  (:use cl
        clack.test
        drakma
        cl-test-more))
(in-package :kawoosh.test.httpd)

(plan 3)

(test-app
 (lambda (env)
   (declare (ignore env))
   `(200 (:content-type "text/plain") ("Hello, Clack!")))
 (lambda ()
   (multiple-value-bind (body status headers)
       (http-request "http://localhost:4242")
     (is status 200)
     (is body "Hello, Clack!")
     (is (cdr (assoc :content-type headers)) "text/plain; charset=utf-8")))
 "Testing simple application")

(finalize)
