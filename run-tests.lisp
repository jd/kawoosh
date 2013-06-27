(defun terminate (status)
  #+sbcl     (sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (   ccl:quit      status)                 ; Clozure CL
  #+clisp    (   ext:quit      status)                 ; GNU CLISP
  #+cmu      (  unix:unix-exit status)                 ; CMUCL
  #+abcl     (   ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (  excl:exit      status :quiet t)        ; Allegro CL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.

(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))
(require 'kawoosh-test)
(kawoosh.dao:drop-tables)
(kawoosh.dao:create-tables)
(clack.test:test-app
 #'kawoosh.httpd:app
 (lambda ()
   (let ((connection (kawoosh.worker:pick-connection)))
     (bordeaux-threads:make-thread
      (lambda () (kawoosh.worker:start connection)))
     (loop while (or (not (kawoosh.dao:connection-network-connection connection))
                     (not (irc::connectedp (kawoosh.dao:connection-network-connection connection)))
                     (not (irc:find-channel (kawoosh.dao:connection-network-connection connection) "#test")))
           do (sleep 0.1))
     (let ((results (5am:run 'kawoosh.test:kawoosh.test)))
       (5am:explain! results)
       (terminate (if (eq (5am:results-status results ) t) 0 1))))))
