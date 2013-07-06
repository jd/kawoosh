(defsystem kawoosh-test
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:kawoosh
               #:clack
               #:clack-test
               #:clack-app-route
               #:postmodern
               #:cl-json
               #:fiveam
               #:drakma
               #:flexi-streams)
  :components
  ((:file "test"
    :pathname "tests/test")
   (:file "httpd"
    :pathname "tests/httpd"
    :depends-on ("test"))
   (:file "worker"
    :pathname "tests/worker"
    :depends-on ("test"))
   (:file "dao"
    :pathname "tests/dao"
    :depends-on ("test"))))
