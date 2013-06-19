(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))
(require 'kawoosh-test)
(postmodern:connect-toplevel "kawoosh" "kawoosh" "kawoosh" "localhost")
(postmodern:execute "SET TIMEZONE='UTC'")
(let ((results (5am:run 5am::*suite*)))
  (5am:explain! results)
  (exit :code (if (eq (5am:results-status results ) t) 0 1)))
