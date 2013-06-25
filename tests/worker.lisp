(defpackage kawoosh.test.worker
  (:use bordeaux-threads
        cl
        fiveam
        kawoosh.dao
        kawoosh.test
        kawoosh.worker))

(in-package :kawoosh.test.worker)

(def-suite kawoosh.test.worker
  :in kawoosh.test
  :description "Kawoosh worker tests")

(in-suite kawoosh.test.worker)

(test irc-connection
  (let ((connection (pick-connection)))
    (make-thread (lambda () (start connection)))
    (loop while (or (not (connection-network-connection connection))
                    (not (irc::connectedp (connection-network-connection connection))))
          do (sleep 0.1))
    (is-true (irc::connectedp (connection-network-connection connection)))))
