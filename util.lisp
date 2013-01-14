(defpackage kawoosh.util
  (:use cl
        cl-async)
  (:export listen-to-fd
           get-socket-fd))

(in-package :cl-async)

(cffi:defcallback fd-cb :void ((fd :int) (what :short) (data-pointer :pointer))
  (declare (ignore fd))
  (let* ((ev (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (read-cb (getf callbacks :read-cb))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (cond
        ((and (< 0 (logand what le:+ev-read+))
              read-cb)
         (funcall read-cb))
        ((and (< 0 (logand what le:+ev-write+))
              write-cb)
         (funcall write-cb))))))

(defun listen-to-fd (fd &key event-cb read-cb write-cb)
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (ev (le:event-new *event-base*
                           fd
                           ;; listen to read/timeout events, and keep listening
                           (logior
                            le:+ev-timeout+
                            (if read-cb le:+ev-read+ 0)
                            (if write-cb le:+ev-write+ 0)
                            le:+ev-persist+)
                           (cffi:callback fd-cb)
                           data-pointer)))
    (save-callbacks data-pointer (list :read-cb read-cb
                                       :write-cb write-cb
                                       :event-cb event-cb))
    (attach-data-to-pointer data-pointer ev)
    (le:event-add ev (cffi:null-pointer))))

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
