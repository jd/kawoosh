(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:bordeaux-threads
               #:cl-irc
               #:cl+ssl
               #:closer-mop
               #:flexi-streams
               #:local-time
               #:cl-postgres+local-time
               #:postmodern
               #:cl-postgres
               #:simple-date
               #:clack
               #:clack-middleware-auth-basic
               #:cl-json
               #:yason)
  :components
  ((:file "dao")
   (:file "util")
   (:file "json")
   (:file "rpc" :depends-on ("dao"))
   (:file "httpd" :depends-on ("dao" "rpc" "json"))
   (:file "worker" :depends-on ("util" "dao"))))
