(defpackage kawoosh.test.worker
  (:use cl
        cl-json
        fiveam
        kawoosh.test))

(in-package :kawoosh.test.worker)

(def-suite kawoosh.test.worker
  :in kawoosh.test
  :description "Kawoosh worker tests")

(in-suite kawoosh.test.worker)

(def-test join-part-channel ()
  (with-fixture database ()
    (with-fixture request ("http://localhost:4242/user/jd"
                           :method :PUT
                           :content (encode-json-to-string '((:name . "jd"))))
      (is (equal '((:name . "jd")) (decode-json stream))))
    (with-fixture request ("http://localhost:4242/server/Naquadah"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "irc.naquadah.org")
                                                             (:ssl . t))))
      (is (equal '((:name . "Naquadah")
                   (:address . "irc.naquadah.org")
                   (:port . 6667)
                   (:ssl . t))
                 (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah"
                           :method :PUT
                           :content (encode-json-to-string '((:nickname . "jd"))))
      (let ((s (decode-json stream)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "Naquadah" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))
    (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel"
                           :expected-status-code 204)
      (is (equal nil (read-char stream nil))))
    (with-fixture worker ("jd" "Naquadah")
      (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test"
                             :method :PUT
                             :content "{}"
                             :expected-status-code 202)
        (is (equal '((:status . "OK") (:message . "Joining channel #test"))
                   (decode-json stream))))
      (worker-wait-for-join "#test")
      (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah/channel/%23test")
        (let ((channel (decode-json stream)))
          (is (equal "#test" (cdr (assoc :name channel))))
          (is (not (equal nil (cdr (assoc :joined-at channel)))))))
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
