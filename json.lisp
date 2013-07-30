(defpackage kawoosh.json
  (:use cl)
  (:export
   decode-json-as-plist
   decode-json-as-plist-only))

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

(defun decode-json-as-plist-only (string fields)
  "Decode on FIELDS in JSON STRING with DECODE-JSON-AS-PLIST."
  (loop :for (key value) :on (decode-json-as-plist string) :by #'cddr
        :when (member key fields)
          :append (list key value)))

