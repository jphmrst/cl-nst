(asdf:operate 'asdf:load-op :nst-simple-tests)

(let ((*print-readably* nil))
  (nst-control-api:run-package :nst-simple-tests))


(let ((logdir
       (merge-pathnames (#+allegro sys:getenv #+sbcl sb-ext:posix-getenv
                                   #+(or clisp clozure) getenv
                                   #-(or allegro sbcl clisp clozure) error
                         "NSTJUNITDIR")
                        (make-pathname :directory (pathname-directory "./")))))
  (format t "~a~%" logdir)
  (nst:junit-results-by-group :dir (namestring logdir)))
