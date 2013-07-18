(defpackage kawoosh.rpc
  (:use cl
        kawoosh.dao
        postmodern)
  (:export rpc-send))

(in-package :kawoosh.rpc)

(defgeneric rpc-send (connection &rest args))

(defmethod rpc-send ((connection connection) &rest args)
  (apply 'rpc-send (connection-id connection) args))

(defmethod rpc-send ((connection-id integer) &rest args)
  (execute (format nil "NOTIFY connection_~a, '~a'"
                   connection-id
                   (with-output-to-string (s) (prin1 args s)))))
