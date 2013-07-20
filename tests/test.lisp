(defpackage kawoosh.test
  (:use bordeaux-threads
        cl
        drakma
        fiveam
        kawoosh.dao
        kawoosh.httpd
        postmodern)
  (:export channel-keys
           kawoosh.test
           request
           database
           worker
           worker-wait-for-join
           worker-wait-for-part))

(in-package :kawoosh.test)

(defvar channel-keys
  '(:name :password :modes :names :topic :joined-at
    :topic-who :topic-time :creation-time))

(def-suite kawoosh.test
  :description "Kawoosh tests")

(def-fixture request (url &key
                          (user "admin")
                          (password "admin")
                          (expected-content-type "application/json")
                          (expected-status-code 200)
                          (method :GET)
                          (content nil))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (http-request (concatenate 'string "http://localhost:4242" url)
                    :basic-authorization (list user password)
                    :want-stream t
                    :method method
                    :content content)
    (declare (ignorable body uri stream must-close reason-phrase))
    (&body)
    (is (equal expected-status-code status-code))
    (is (equal expected-content-type (cdr (assoc :content-type headers))))))

(def-fixture database ()
  (drop-tables)
  (create-tables)
  (&body)
  (drop-tables))

(def-fixture worker (username server)
  (let ((connection
          (with-pg-connection
              (car (select-dao 'connection (:and (:= 'username username)
                                                 (:= 'server server)))))))
    (assert connection nil "No such fixture connection")
    (let ((th (make-thread (lambda () (kawoosh.worker:start connection))
                           :name "Kawoosh worker")))
      (defun worker-wait-for-join (channel)
        (loop until (with-pg-connection
                        (get-dao 'channel (connection-id connection) channel))
              do (sleep 0.1)))
      (defun worker-wait-for-part (channel)
        (loop while (with-pg-connection
                        (get-dao 'channel (connection-id connection) channel))
              do (sleep 0.1)))
      ;; Wait for the connection to be established
      (loop until (connection-connected-p connection)
            do (sleep 0.1))
      (&body)
      (destroy-thread th))))

