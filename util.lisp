(defpackage kawoosh.util
  (:use cl)
  (:export get-socket-fd))

(in-package :kawoosh.util)

(defun get-socket-fd (connection)
  "Gets the fd from a socket connection."
  (let* ((type (type-of connection))
         (stream (cond ((subtypep type 'usocket:stream-usocket)
                        (usocket:socket-stream connection))
                       ((subtypep type 'stream)
                        connection))))
    (when stream
      #+sbcl
      (sb-sys:fd-stream-fd stream)
      #+cmu
      (system:fd-stream-fd stream)
      #+ccl
      (ccl::ioblock-device (ccl::stream-ioblock stream t))
      #+clisp
      (ext:stream-handles stream))))
