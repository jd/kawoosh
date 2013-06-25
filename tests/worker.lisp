(defpackage kawoosh.test.worker
  (:use bordeaux-threads
        cl
        fiveam
        kawoosh.dao
        kawoosh.test
        kawoosh.worker)
  (:export irc-connection))

(in-package :kawoosh.test.worker)

(def-suite kawoosh.test.worker
  :in kawoosh.test
  :description "Kawoosh worker tests")

(in-suite kawoosh.test.worker)

(test irc-connection
  (is-true t))
