(defpackage kawoosh.test.httpd
  (:use bordeaux-threads
        cl
        cl-json
        clack.test
        drakma
        kawoosh.dao
        kawoosh.httpd
        kawoosh.test
        fiveam
        postmodern))

(in-package :kawoosh.test.httpd)

(def-suite kawoosh.test.httpd
  :in kawoosh.test
  :description "Kawoosh HTTPD tests")

(in-suite kawoosh.test.httpd)

;; XXX Write as a fixture
;; XXX Add a check for Content-Type to factorize the code
(defmacro with-request (req &rest body)
  `(multiple-value-bind (body status headers uri stream must-close reason-phrase)
       ,(if (listp req)
            `(http-request ,(cadr req)
                           :want-stream t
                           :method ,(car req)
                           :content ,(caddr req))
            `(http-request ,req
                           :want-stream t))
     (declare (ignorable body status headers uri stream must-close reason-phrase))
     ,@body))

(defvar channel-keys
  '(:name :password :modes :names :topic
    :topic--who :topic--time :creation--time))

(def-fixture database ()
  (kawoosh.dao:drop-tables)
  (kawoosh.dao:create-tables)
  (&body)
  (kawoosh.dao:drop-tables))

(def-fixture worker (username server)
  (let ((connection
          (with-pg-connection
              (car (select-dao 'connection (:and (:= 'username username)
                                                 (:= 'server server)))))))
    (assert connection nil "No such fixture connection")
    (let ((th (make-thread (lambda () (kawoosh.worker:start connection)))))
      (defun worker-wait-for-join (channel)
        (loop until (find channel
                          (mapcar 'irc:normalized-name
                                  (irc:channels (irc:user (connection-network-connection connection))))
                          :test #'string=)
              do (sleep 0.1)))
      (defun worker-wait-for-part (channel)
        (loop while (find channel
                               (mapcar 'irc:normalized-name
                                       (irc:channels (irc:user (connection-network-connection connection))))
                               :test #'string=)
              do (sleep 0.1)))
      ;; Wait for the connection to be established
      (loop until (connection-connected-p connection)
            do (sleep 0.1))
      (&body)
      (destroy-thread th))))


(test
 httpd-user
 (with-fixture database ()
   (with-request (:delete "http://localhost:4242/user/jd")
     (is (equal 200 status))
     (is (equal nil (read-line stream nil)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request "http://localhost:4242/user"
     (is (equal 200 status))
     (is (equal nil (read-line stream nil)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request (:put "http://localhost:4242/user/jd")
     (is (equal 200 status))
     (is (equal '((:name . "jd")) (decode-json stream)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request "http://localhost:4242/user"
     (is (equal 200 status))
     (is (equal '(((:name . "jd"))) (decode-json stream)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request (:delete "http://localhost:4242/user/jd")
     (is (equal 200 status))
     (is (equal nil (read-line stream nil)))
     (is (equal  "application/json" (cdr (assoc :content-type headers)))))
   (with-request "http://localhost:4242/user"
     (is (equal 200 status))
     (is (equal nil (read-line stream nil)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request
     "http://localhost:4242/user/foobar"
     (is (equal 404 status))
     (is (equal '((:status . "Not Found") (:message . "No such user")) (decode-json stream)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))))

(test
 httpd-user-events-retrieval
 (with-fixture database ()
   (with-fixture worker ("jd" "Naquadah")
     (with-request "http://localhost:4242/user/jd/events"
       (is (equal status 200))
       (let ((event (decode-json-from-string (read-line stream nil))))
         (is (equal "NOTICE" (cdr (assoc :command event))))
         (is (equal "irc.naquadah.org" (cdr (assoc :source event)))))
       (is (equal "application/json" (cdr (assoc :content-type headers))))))
   (with-request "http://localhost:4242/user/nosuchuser/events"
     (is (equal 404 status))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))))

(test
 httpd-server
 (with-fixture database ()
   (with-request "http://localhost:4242/server"
     (is (equal 200 status))
     (is (equal '(((:name . "Naquadah")
                   (:address . "irc.naquadah.org")
                   (:port . 6667)
                   (:ssl . t)))
                (decode-json stream) ))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request
     "http://localhost:4242/server/Naquadah"
     (is (equal 200 status))
     (is (equal '((:name . "Naquadah")
                  (:address . "irc.naquadah.org")
                  (:port . 6667)
                  (:ssl . t))
                (decode-json stream)))
     (is (equal "application/json" (cdr (assoc :content-type headers)) )))
   (with-request
     "http://localhost:4242/server/foobar"
     (is (equal 404 status))
     (is (equal '((:status . "Not Found") (:message . "No such server"))
                (decode-json stream) ))
     (is (equal "application/json"
                (cdr (assoc :content-type headers)) )))))

(test
 httpd-user-connection-list
 (with-fixture database ()
   (with-request
     "http://localhost:4242/user/jd/connection"
     (is (equal 200 status))
     (let* ((data (decode-json stream))
            (s (first data)))
       (is (equal 1 (length data)))
       (is (equal nil
                  (set-exclusive-or (mapcar 'car s)
                                    '(:server :username :nickname :current--nickname
                                      :realname :connected :motd :network-connection))))
       (is (equal "Naquadah" (cdr (assoc :server s))))
       (is (equal "Julien Danjou" (cdr (assoc :realname s))))
       (is (equal "jd" (cdr (assoc :nickname s))))
       (is (equal "jd" (cdr (assoc :username s)))))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah"
     (is (equal 200 status))
     (let* ((s (decode-json stream)))
       (is (equal nil
                  (set-exclusive-or (mapcar 'car s)
                                    '(:server :username :nickname :current--nickname
                                      :realname :connected :motd :network-connection))))
       (is (equal  "Naquadah" (cdr (assoc :server s))))
       (is (equal "Julien Danjou" (cdr (assoc :realname s))))
       (is (equal "jd" (cdr (assoc :nickname s))))
       (is (equal "jd" (cdr (assoc :username s)))))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))))

(test
 http-user-channel
 (with-fixture database ()
   (with-request
     "http://localhost:4242/user/jd/connection/Naquadah/channel"
     (is (equal 200 status))
     (is (equal nil (read-char stream nil)))
     (is (equal "application/json" (cdr (assoc :content-type headers)))))
   (with-fixture worker ("jd" "Naquadah")
     (with-request
         (:put "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
       (is (equal 202 status))
       (is (equal '((:status . "OK") (:message . "Joining channel #test"))
                  (decode-json stream)))
       (is (equal "application/json" (cdr (assoc :content-type headers)))))
     (worker-wait-for-join "#test")
     (with-request
         (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
       (is (equal 202 status))
       (is (equal '((:status . "OK") (:message . "Parting channel #test"))
                  (decode-json stream)))
       (is (equal  "application/json" (cdr (assoc :content-type headers)))))
     (worker-wait-for-part "#test")
     (with-request
         (:put "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test" "{}")
       (is (equal 202 status))
       (is (equal '((:status . "OK") (:message . "Joining channel #test"))
                  (decode-json stream)))
       (is (equal "application/json" (cdr (assoc :content-type headers)))))
     (worker-wait-for-join "#test")
     (with-request
       "http://localhost:4242/user/jd/connection/Naquadah/channel"
       (is (equal 200 status))
       (let* ((data (decode-json stream))
              (c (car data )))
         (is (equal 1 (length data)))
         (is (equal nil (set-exclusive-or (mapcar 'car c) channel-keys)))
         (is (equal "#test" (cdr (assoc :name c)))))
       (is (equal "application/json" (cdr (assoc :content-type headers)))))
     (with-request
       "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
       (is (equal 200 status))
       (let ((c (decode-json stream)))
         (is (equal nil (set-exclusive-or (mapcar 'car c) channel-keys)))
         (is (equal "#test" (cdr (assoc :name c)))))
       (is (equal "application/json" (cdr (assoc :content-type headers)))))
     (with-request
         (:put "http://localhost:4242/user/jd/connection/NoConnection/channel/%23test" "{}")
       (is (equal 404 status))
       (is (equal '((:status . "Not Found") (:message . "No such connection"))
                  (decode-json stream)))
       (is (equal  "application/json" (cdr (assoc :content-type headers)))))
     (with-request
         (:delete "http://localhost:4242/user/jd/connection/Naquadah/channel/foobar" "{}")
       (is (equal 404 status))
       (is (equal '((:status . "Not Found")
                    (:message . "No such connection or channel not joined"))
                  (decode-json stream)))
       (is (equal  "application/json" (cdr (assoc :content-type headers)))))
     (with-request
       "http://localhost:4242/user/jd/connection/Naquadah/channel/%23test/events"
       (is (equal 200 status))
       (let* ((s (decode-json stream))
              (event (nth 0 s)))
         (is (equal "JOIN" (cdr (assoc :command event))))
         (is (equal nil (cdr (assoc :payload event))))
         (is (equal "#test" (cdr (assoc :target event)))))
       (is (equal  "application/json" (cdr (assoc :content-type headers))))))))

(test start-stop "Kawoosh httpd start and stop"
      (start)
      (stop)
      (stop (start)))
