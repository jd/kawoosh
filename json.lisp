(defpackage kawoosh.json
  (:use cl)
  (:export decode-json-as-plist))

(in-package :kawoosh.json)

(defun intern-keyword (name)
  "Intern NAME as keyword"
  (intern (string-upcase name) :keyword))

(defun decode-json-as-plist (string)
  "Decode JSON STRING and returns the result as a plist.
Booleans are encoded as 'yason:true or 'yason:false."
  (let ((yason:*parse-json-booleans-as-symbols* t)
        (yason:*parse-object-as* :plist)
        (yason:*parse-object-key-fn* #'intern-keyword))
    (yason:parse string)))

