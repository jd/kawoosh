(defpackage kawoosh.test.dao
  (:use cl
        fiveam
        kawoosh.dao
        kawoosh.test))

(in-package :kawoosh.test.dao)

(def-suite kawoosh.test.dao
  :in kawoosh.test
  :description "Kawoosh DAO tests")

(in-suite kawoosh.test.dao)

(def-test user-has-access ()
  (let ((user (make-instance 'user :name "foobar" :admin-p nil)))
    (is-true (user-has-access-p user 'any))
    (is-false (user-has-access-p user 'unknown-acl))
    (is-true (user-has-access-p user 'user "foobar"))
    (is-false (user-has-access-p user 'user "fOObar"))
    (is-false (user-has-access-p user 'user "Foobar"))
    (is-false (user-has-access-p user 'user "lolilol"))
    (is-false (user-has-access-p user 'admin))))

(def-test user-has-access-admin ()
  (let ((user (make-instance 'user :name "foobar" :admin-p t)))
    (is-true (user-has-access-p user 'any))
    (is-true (user-has-access-p user 'unknown-acl))
    (is-true (user-has-access-p user 'user "foobar"))
    (is-true (user-has-access-p user 'user "fOObar"))
    (is-true (user-has-access-p user 'user "Foobar"))
    (is-true (user-has-access-p user 'user "lolilol"))
    (is-true (user-has-access-p user 'admin))))
