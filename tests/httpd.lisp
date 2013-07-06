(defpackage kawoosh.test.httpd
  (:use cl
        cl-json
        drakma
        kawoosh.httpd
        kawoosh.test
        fiveam))

(in-package :kawoosh.test.httpd)

(def-suite kawoosh.test.httpd
  :in kawoosh.test
  :description "Kawoosh HTTPD tests")

(in-suite kawoosh.test.httpd)

(def-test httpd-authorization ()
  (with-fixture database ()
    (with-fixture request ("http://localhost:4242/"
                           :user "foobar"
                           :password ""
                           :expected-content-type "text/plain; charset=utf-8"
                           :expected-status-code 401))))

(def-test httpd-user ()
  (with-fixture database ()
    (with-fixture request ("http://localhost:4242/user/jd"
                           :method :PUT
                           :content (encode-json-to-string '((:password . "f00b4r"))))
      (is (equal '((:name . "jd")) (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user")
      (is (equal '(((:name . "admin"))
                   ((:name . "jd")))
                 (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user/jd")
      (is (equal '((:name . "jd"))
                 (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user/jd"
                           :user "jd"
                           :password "f00b4r")
      (is (equal '((:name . "jd"))
                 (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user/jd"
                           :user "jd"
                           :password "f00b4r"
                           :method :DELETE
                           :expected-status-code 403))
    (with-fixture request ("http://localhost:4242/user"
                           :user "jd"
                           :password "f00b4r"
                           :expected-status-code 403))
    (with-fixture request ("http://localhost:4242/user/jd"
                           :method :DELETE
                           :expected-status-code 204)
      (is (equal nil (read-line stream nil))))
    (with-fixture request ("http://localhost:4242/user")
      (is (equal '(((:name . "admin")))
                 (decode-json stream))))
    (with-fixture request ("http://localhost:4242/user/foobar" :expected-status-code 404)
      (is (equal '((:status . "Not Found") (:message . "No such user")) (decode-json stream))))))

(def-test httpd-user-events-retrieval ()
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
    (with-fixture worker ("jd" "Naquadah")
      (with-fixture request ("http://localhost:4242/user/jd/events")
        (let ((event (decode-json-from-string (read-line stream nil))))
          (is (equal "NOTICE" (cdr (assoc :command event))))
          (is (equal "irc.naquadah.org" (cdr (assoc :source event))))))
      (with-fixture request ("http://localhost:4242/user/nosuchuser/events" :expected-status-code 404)))))

(def-test httpd-server ()
  (with-fixture database ()
    (with-fixture request ("http://localhost:4242/server/Naquadah"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "irc.naquadah.org")
                                                             (:ssl . t))))
      (is (equal '((:name . "Naquadah")
                   (:address . "irc.naquadah.org")
                   (:port . 6667)
                   (:ssl . t))
                 (decode-json stream))))
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

(def-test httpd-user-connection ()
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
    (with-fixture request ("http://localhost:4242/user/jd/connection")
      (let* ((data (decode-json stream))
             (s (first data)))
        (is (equal 1 (length data)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "Naquadah" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))
    (with-fixture request ("http://localhost:4242/user/jd/connection/Naquadah")
      (let ((s (decode-json stream)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "Naquadah" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))))
