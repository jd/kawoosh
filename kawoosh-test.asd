(defsystem kawoosh-test
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:kawoosh
               #:clack
               #:clack-test
               #:clack-app-route
               #:postmodern
               #:cl-json
               #:cl-test-more
               #:drakma
               #:flexi-streams)
  :components
  ((:module test
    :components ((:file "httpd")))))
