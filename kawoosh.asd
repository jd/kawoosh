(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:cl-async
               #:cl-async-ssl
               #:cl-irc
               #:postmodern
               #:simple-date
               #:clack
               #:clack-app-route
               #:cl-json)
  :components
  ((:file "dao")
   (:file "httpd" :depends-on ("dao"))
   (:file "worker" :depends-on ("dao")))
