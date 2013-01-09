(defsystem kawoosh-test
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:clack
               #:clack-test
               #:clack-app-route
               #:postmodern
               #:cl-json
               #:cl-test-more
               #:drakma)
  :components
  ((:file "dao")
   (:file "httpd" :depends-on ("dao"))
   (:module test
    :components ((:file "httpd")))))
