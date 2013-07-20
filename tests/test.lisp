(defpackage kawoosh.test
  (:use bordeaux-threads
        cl
        drakma
        fiveam
        kawoosh.dao
        kawoosh.httpd
        postmodern)
  (:export *httpd-test-port*
           channel-keys
           kawoosh.test
           request
           database
           worker
           worker-wait-for-join
           worker-wait-for-part))

(in-package :kawoosh.test)

(defparameter *httpd-test-port* 4242)

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
      (http-request (format nil "http://localhost:~a~a" *httpd-test-port* url)
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
    (make-thread (lambda () (kawoosh.worker:start connection))
                 :name "Kawoosh worker")
    (macrolet ((worker-wait-for-join (channel)
                 `(loop until (with-pg-connection
                                  (get-dao 'channel (connection-id connection) ,channel))
                        do (sleep 0.1)))
               (worker-wait-for-part (channel)
                 `(loop while (with-pg-connection
                                  (get-dao 'channel (connection-id connection) ,channel))
                        do (sleep 0.1))))
      ;; Wait for the connection to be established
      (loop until (connection-connected-p connection)
            do (sleep 0.1))
      (&body)
      (irc:quit (connection-network-connection connection) "I've finished my test!"))))

