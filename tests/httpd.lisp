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

(defvar channel-keys
  '(:name :password :modes :names :topic
    :topic--who :topic--time :creation--time))

(def-fixture request (url &key
                          (expected-content-type "application/json")
                          (expected-status-code 200)
                          (method :GET)
                          (content nil))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (http-request url
                    :want-stream t
                    :method method
                    :content content)
    (declare (ignorable body uri stream must-close reason-phrase))
    (&body)
    (is (equal expected-status-code status-code))
    (is (equal expected-content-type (cdr (assoc :content-type headers))))))

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
   (with-fixture request ("http://localhost:4242/user/jd" :method :DELETE)
     (is (equal nil (read-line stream nil))))
   (with-fixture request ("http://localhost:4242/user")
     (is (equal nil (read-line stream nil))))
   (with-fixture request ("http://localhost:4242/user/jd" :method :PUT)
     (is (equal '((:name . "jd")) (decode-json stream))))
   (with-fixture request ("http://localhost:4242/user")
     (is (equal '(((:name . "jd"))) (decode-json stream))))
   (with-fixture request ("http://localhost:4242/user/jd" :method :DELETE)
     (is (equal nil (read-line stream nil))))
   (with-fixture request ("http://localhost:4242/user")
     (is (equal nil (read-line stream nil))))
   (with-fixture request ("http://localhost:4242/user/foobar" :expected-status-code 404)
     (is (equal '((:status . "Not Found") (:message . "No such user")) (decode-json stream))))))

(test
 httpd-user-events-retrieval
 (with-fixture database ()
   (with-fixture worker ("jd" "Naquadah")
     (with-fixture request ("http://localhost:4242/user/jd/events")
       (let ((event (decode-json-from-string (read-line stream nil))))
         (is (equal "NOTICE" (cdr (assoc :command event))))
         (is (equal "irc.naquadah.org" (cdr (assoc :source event))))))
   (with-fixture request ("http://localhost:4242/user/nosuchuser/events" :expected-status-code 404)))))

(test
 httpd-server
 (with-fixture database ()
   (with-fixture request ("http://localhost:4242/server")
     (is (equal '(((:name . "Naquadah")
                   (:address . "irc.naquadah.org")
                   (:port . 6667)
                   (:ssl . t)))
                (decode-json stream) )))
   (with-fixture request ("http://localhost:4242/server/Naquadah")
     (is (equal '((:name . "Naquadah")
                  (:address . "irc.naquadah.org")
                  (:port . 6667)
                  (:ssl . t))
                (decode-json stream))))
   (with-fixture request ("http://localhost:4242/server/foobar" :expected-status-code 404)
     (is (equal '((:status . "Not Found") (:message . "No such server"))
                (decode-json stream) )) )))

(test
 httpd-user-connection-list
 (with-fixture database ()
   (with-fixture request ("http://localhost:4242/user/jd/connection")
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
       (is (equal "jd" (cdr (assoc :username s))))))
   (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah")
     (let ((s (decode-json stream)))
       (is (equal nil
                  (set-exclusive-or (mapcar 'car s)
                                    '(:server :username :nickname :current--nickname
                                      :realname :connected :motd :network-connection))))
       (is (equal  "Naquadah" (cdr (assoc :server s))))
       (is (equal "Julien Danjou" (cdr (assoc :realname s))))
       (is (equal "jd" (cdr (assoc :nickname s))))
       (is (equal "jd" (cdr (assoc :username s))))))))

(test
 http-user-channel
 (with-fixture database ()
   (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel")
     (is (equal nil (read-char stream nil))))
   (with-fixture worker ("jd" "Naquadah")
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
                            :method :PUT
                            :content "{}"
                            :expected-status-code 202)
       (is (equal '((:status . "OK") (:message . "Joining channel #test"))
                  (decode-json stream))))
     (worker-wait-for-join "#test")
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
                            :method :DELETE
                            :content "{}"
                            :expected-status-code 202)
       (is (equal '((:status . "OK") (:message . "Parting channel #test"))
                  (decode-json stream))))
     (worker-wait-for-part "#test")
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
                            :method :PUT
                            :content "{}"
                            :expected-status-code 202)
       (is (equal '((:status . "OK") (:message . "Joining channel #test"))
                  (decode-json stream))))
     (worker-wait-for-join "#test")
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel")
       (let* ((data (decode-json stream))
              (c (car data )))
         (is (equal 1 (length data)))
         (is (equal nil (set-exclusive-or (mapcar 'car c) channel-keys)))
         (is (equal "#test" (cdr (assoc :name c))))))
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test")
       (let ((c (decode-json stream)))
         (is (equal nil (set-exclusive-or (mapcar 'car c) channel-keys)))
         (is (equal "#test" (cdr (assoc :name c))))))
     (with-fixture request ("http://localhost:4242/user/jd/connection/NoConnection/channel/%23test"
                            :method :PUT
                            :content "{}"
                            :expected-status-code 404)
       (is (equal '((:status . "Not Found") (:message . "No such connection"))
                  (decode-json stream))))
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/foobar"
                            :method :DELETE
                            :content "{}"
                            :expected-status-code 404)
       (is (equal '((:status . "Not Found")
                    (:message . "No such connection or channel not joined"))
                  (decode-json stream))))
     (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test/events")
       (let* ((s (decode-json stream))
              (event (nth 0 s)))
         (is (equal "JOIN" (cdr (assoc :command event))))
         (is (equal nil (cdr (assoc :payload event))))
         (is (equal "#test" (cdr (assoc :target event)))))))))

(test start-stop "Kawoosh httpd start and stop"
      (start)
      (stop)
      (stop (start)))
