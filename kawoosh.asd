(defsystem kawoosh
  :author "Julien Danjou <julien@danjou.info>"
  :description "IRC/HTTP gateway"
  :depends-on (#:bordeaux-threads
               #:cl-irc
               #:cl+ssl
               #:flexi-streams
               #:postmodern
               #:cl-postgres
               #:simple-date
               #:clack
               #:clack-middleware-auth-basic
               #:cl-json
               #:usocket)
  :components
  ((:file "dao")
   (:file "util")
   (:file "rpc" :depends-on ("dao"))
   (:file "httpd" :depends-on ("dao" "rpc"))
   (:file "worker" :depends-on ("util" "dao"))))
