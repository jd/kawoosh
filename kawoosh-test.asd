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
  ((:module tests
    :components ((:file "httpd")))))
