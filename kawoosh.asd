(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:bordeaux-threads
               #:cl-irc
               #:cl+ssl
               #:flexi-streams
               #:local-time
               #:cl-postgres+local-time
               #:postmodern
               #:cl-postgres
               #:simple-date
               #:clack
               #:clack-middleware-auth-basic
               #:cl-json
               #:usocket
               #:yason)
  :components
  ((:file "dao")
   (:file "util")
   (:file "rpc" :depends-on ("dao"))
   (:file "httpd" :depends-on ("dao" "rpc"))
   (:file "worker" :depends-on ("util" "dao"))))
