(push +CLOSER-DIRECTORY+ asdf:*central-registry*)
(push (merge-pathnames #p"" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"ext/defdoc/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"ext/defcontract/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/asdf/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/direct/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/manual/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/meta/" +NST-DIRECTORY+) asdf:*central-registry*)
(push (merge-pathnames #p"test/util/" +NST-DIRECTORY+) asdf:*central-registry*)
#+clozure-common-lisp (setf *debugger-hook* #'(lambda (cnd prev) (quit 1)))
#+sbcl (setf *debugger-hook* #'(lambda (cnd prev) (exit :unix-status 1)))

(asdf:oos 'asdf:load-op :nst)
(asdf:oos 'asdf:load-op :asdf-nst)
(setf nst::*nst-verbosity* 0)
