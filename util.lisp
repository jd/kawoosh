(defpackage kawoosh.util
  (:use cl)
  (:export list->array))

(in-package :kawoosh.util)

;; XXX unit test me
(defun list->array (l)
  "Recursive list to array conversion."
  (make-array (length l) :initial-contents l))
