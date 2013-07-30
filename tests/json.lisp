(defpackage kawoosh.test.json
  (:use cl
        fiveam
        kawoosh.json
        kawoosh.test))

(in-package :kawoosh.test.json)

(def-suite kawoosh.test.json
  :in kawoosh.test
  :description "Kawoosh JSON tests")

(in-suite kawoosh.test.json)

(def-test json-decode-as-plist ()
  (let ((d (decode-json-as-plist "{\"t\": true, \"f\": false, \"foo\": \"bar\"}")))
    (is (equal 'yason:true (getf d :t)))
    (is (equal 'yason:false (getf d :f)))
    (is (equal "bar" (getf d :foo)))))

(def-test json-decode-as-plist-only ()
  (let ((d (decode-json-as-plist-only
            "{\"t\": true, \"f\": false, \"foo\": \"bar\"}"
            '(:foo))))
    (is (equal d '(:foo "bar")))))
