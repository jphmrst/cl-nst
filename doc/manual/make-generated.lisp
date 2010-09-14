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
;; (trace defdoc::output-latex defdoc::spec-to-latex)
(setf defdoc::*defdoc-latex-default-directory* #p"./gen/")
(defun process (symbol usage)
  (format t "Writing ~a to ~a.tex...~%" usage symbol)
  (defdoc::write-spec-latex symbol usage))
(process 'nst:def-fixtures   :macro)
(process 'nst:def-test-group :macro)
(format t "Exiting~%")
