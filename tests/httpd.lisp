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
    (with-fixture request ("/"
                           :user "foobar"
                           :password ""
                           :expected-content-type "text/plain; charset=utf-8"
                           :expected-status-code 401))))

(def-test page-not-found ()
  (with-fixture database ()
    (with-fixture request ("/thismaynotexist"
                           :expected-content-type nil
                           :expected-status-code 404))))

(def-test httpd-server ()
  (with-fixture database ()
    (with-fixture request ("/user/user"
                           :method :PUT
                           :content (encode-json-to-string '((:password . "f00b4r"))))
      (is (equal '((:name . "user")) (decode-json stream))))
    (with-fixture request ("/server/localhost"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "localhost"))))
      (is (equal '((:name . "localhost")
                   (:address . "localhost")
                   (:port . 6667)
                   (:ssl))
                 (decode-json stream))))
    (with-fixture request ("/server/localhost"
                           :user "user"
                           :password "f00b4r"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "localhost")))
                           :expected-status-code 403))
    (with-fixture request ("/server"
                           :user "user"
                           :password "f00b4r")
      (is (equal '(((:name . "localhost")
                    (:address . "localhost")
                    (:port . 6667)
                    (:ssl)))
                 (decode-json stream))))
    (with-fixture request ("/server/localhost"
                           :user "user"
                           :password "f00b4r")
      (is (equal '((:name . "localhost")
                   (:address . "localhost")
                   (:port . 6667)
                   (:ssl))
                 (decode-json stream))))
    (with-fixture request ("/server/foobar" :expected-status-code 404)
      (is (equal '((:status . "Not Found") (:message . "No such server"))
                 (decode-json stream))))))

(def-test httpd-user ()
  (with-fixture database ()
    (with-fixture request ("/user/jd"
                           :method :PUT
                           :content (encode-json-to-string '((:password . "f00b4r"))))
      (is (equal '((:name . "jd")) (decode-json stream))))
    (with-fixture request ("/user")
      (is (equal '(((:name . "admin"))
                   ((:name . "jd")))
                 (decode-json stream))))
    (with-fixture request ("/user/jd")
      (is (equal '((:name . "jd"))
                 (decode-json stream))))
    (with-fixture request ("/user/jd"
                           :user "jd"
                           :password "f00b4r")
      (is (equal '((:name . "jd"))
                 (decode-json stream))))
    (with-fixture request ("/user/jd"
                           :user "jd"
                           :password "f00b4r"
                           :method :DELETE
                           :expected-status-code 403))
    (with-fixture request ("/user"
                           :user "jd"
                           :password "f00b4r"
                           :expected-status-code 403))
    (with-fixture request ("/user/jd"
                           :method :DELETE
                           :expected-status-code 204)
      (is (equal nil (read-line stream nil))))
    (with-fixture request ("/user")
      (is (equal '(((:name . "admin")))
                 (decode-json stream))))
    (with-fixture request ("/user/foobar" :expected-status-code 404)
      (is (equal '((:status . "Not Found") (:message . "No such user")) (decode-json stream))))))

(def-test httpd-user-events-retrieval ()
  (with-fixture database ()
    (with-fixture request ("/user/jd"
                           :method :PUT
                           :content (encode-json-to-string '((:name . "jd"))))
      (is (equal '((:name . "jd")) (decode-json stream))))
    (with-fixture request ("/server/localhost"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "localhost"))))
      (is (equal '((:name . "localhost")
                   (:address . "localhost")
                   (:port . 6667)
                   (:ssl))
                 (decode-json stream))))
    (with-fixture request ("/user/jd/connection/localhost"
                           :method :PUT
                           :content (encode-json-to-string '((:nickname . "jd"))))
      (let ((s (decode-json stream)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "localhost" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))
    (with-fixture worker ("jd" "localhost")
      (with-fixture request ("/user/jd/connection/localhost/event")
        (let ((event (decode-json-from-string (read-line stream nil))))
          (is (equal "RPL_HELLO" (cdr (assoc :command event))))
          (is (equal "irc.localhost" (cdr (assoc :source event))))
          (let ((event-id (cdr (assoc :id event))))
            (with-fixture request ((format nil "/user/jd/event/~a" event-id))
              (let ((event-by-id (decode-json-from-string (read-line stream nil))))
                (is (equal "RPL_HELLO" (cdr (assoc :command event-by-id))))
                (is (equal "irc.localhost" (cdr (assoc :source event-by-id)))))))))
      (with-fixture request ("/user/jd/connection/localhost/event?from=foobar"
                             :expected-status-code 400)
        (let ((err (decode-json stream)))
          (is (equal "Bad Request" (cdr (assoc :status err))))
          (is (equal "Failed to parse \"foobar\" as an rfc3339 time: NIL" (cdr (assoc :message err))))))
      ;; At that time it might make the test fail, but I'm sure I won't
      ;; care. Bender will fix it for me.
      (with-fixture request ("/user/jd/connection/localhost/event?from=3000-01-01")
        (is (equal 'eof (read-line stream nil 'eof)))))
    (with-fixture request ("/user/nosuchuser/connection/localhost/event" :expected-status-code 404))))

(def-test httpd-user-connection ()
  (with-fixture database ()
    (with-fixture request ("/user/jd"
                           :method :PUT
                           :content (encode-json-to-string '((:name . "jd"))))
      (is (equal '((:name . "jd")) (decode-json stream))))
    (with-fixture request ("/server/localhost"
                           :method :PUT
                           :content (encode-json-to-string '((:address . "localhost"))))
      (is (equal '((:name . "localhost")
                   (:address . "localhost")
                   (:port . 6667)
                   (:ssl))
                 (decode-json stream))))
    (with-fixture request ("/user/jd/connection/localhost"
                           :method :PUT
                           :content (encode-json-to-string '((:nickname . "jd"))))
      (let ((s (decode-json stream)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "localhost" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))
    (with-fixture request ("/user/jd/connection")
      (let* ((data (decode-json stream))
             (s (first data)))
        (is (equal 1 (length data)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "localhost" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))
    (with-fixture request ("/user/jd/connection/localhost")
      (let ((s (decode-json stream)))
        (is (equal nil
                   (set-exclusive-or (mapcar 'car s)
                                     '(:server :username :nickname :current-nickname
                                       :realname :connected :motd :network-connection))))
        (is (equal "localhost" (cdr (assoc :server s))))
        (is (equal "jd" (cdr (assoc :realname s))))
        (is (equal "jd" (cdr (assoc :nickname s))))
        (is (equal "jd" (cdr (assoc :username s))))))))
