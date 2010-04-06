(asdf:operate 'asdf:load-op :nst-manual-tests)

(let ((*print-readably* nil))
  (nst-control-api:run-group 'nst-manual::failures)
  (nst-control-api:run-group 'nst-manual::some-magic)
  (nst-control-api:run-group 'nst-manual::core-checks-sub))


(let ((logdir
       (merge-pathnames (#+allegro sys:getenv
                                   #+sbcl sb-ext:posix-getenv
                                   #+(or clisp clozure) getenv
                                   #+lispworks lispworks:environment-variable
                                   #-(or allegro sbcl clisp clozure lispworks) error
                         "NSTJUNITDIR")
                        (make-pathname :directory (pathname-directory "./")))))
  (format t "~a~%" logdir)
  (nst:junit-results-by-group :dir (namestring logdir)))
