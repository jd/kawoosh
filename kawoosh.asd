(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:cl-async
               #:cl-async-ssl
               #:cl-irc
               #:postmodern
               #:cl-postgres
               #:simple-date
               #:clack
               #:clack-app-route
               #:bordeaux-threads
               #:cl-json)
  :components
  ((:file "dao")
   (:file "httpd" :depends-on ("dao"))
   (:file "worker" :depends-on ("dao"))))
