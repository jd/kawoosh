(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:cl-async
               #:cl-async-ssl
               #:cl-irc
               #:flexi-streams
               #:postmodern
               #:cl-postgres
               #:simple-date
               #:clack
               #:clack-app-route
               #:cl-json
               #:split-sequence
               #:usocket)
  :components
  ((:file "dao")
   (:file "util")
   (:file "httpd" :depends-on ("dao"))
   (:file "worker" :depends-on ("util" "dao"))))
