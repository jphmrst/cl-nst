#!/usr/local/allegro/acl82/mlisp -#!

(format t "Starting up...~%")
(require :asdf)
(setf asdf:*central-registry*
      (list* #P"../../"
             #P"../../ext/defdoc/"
             #P"~/Lib/Lisp/SIFT/closer-mop/"
             asdf:*central-registry*))
(format t "Loading NST...~%")
(asdf:oos 'asdf:load-op :nst)
(setf defdoc::*defdoc-latex-default-directory* #p"./gen/")
(defun process (symbol usage)
  (format t "Writing ~a ~a...~%" usage symbol)
  (defdoc:write-spec-latex symbol usage))

(process 'nst:def-fixtures   'compiler-macro)
(process 'nst:def-test-group 'compiler-macro)
(process 'nst:make-success-report 'function)
(process 'nst:*nst-output-stream* 'variable)
(process 'nst:nst-results 'method-combination)
(format t "Exiting~%")
