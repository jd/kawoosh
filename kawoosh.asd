(asdf:defsystem kawoosh
  :author "Julin Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:cl-async
               #:cl-async-ssl
               #:cl-irc
               #:postmodern
               #:simple-date
               #:clack
               #:clack-app-route)
  :components
  ((:file "dao")
   (:file "httpd" :depends-on ("dao"))
   (:file "worker" :depends-on ("dao"))))
