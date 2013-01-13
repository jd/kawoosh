(require :kawoosh)
(postmodern:connect-toplevel "kawoosh" "kawoosh" "kawoosh" "localhost")
(postmodern:execute "SET TIMEZONE='UTC'")
(kawoosh.worker:start)
