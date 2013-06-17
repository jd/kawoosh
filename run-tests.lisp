(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))
(print asdf:*central-registry*)
(require 'kawoosh-test)
(postmodern:connect-toplevel "kawoosh" "kawoosh" "kawoosh" "localhost")
(postmodern:execute "SET TIMEZONE='UTC'")
(cl-test-more:run-test-package 'kawoosh.test.httpd)
(quit)
