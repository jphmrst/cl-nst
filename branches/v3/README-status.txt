
Look at sample.lisp for the working snippet.

[April 2008]
The order of calls is now correct.  To verify:

 :als nst
 :pa nst
 :cd Lib/Lisp/3nst  ; or wherever you keep it
 :cl sample

 ;; Execution with group.
 (setf gr2b-inst (make-instance (group-class-name 'mnst::gr2b)))
 (run gr2b-inst)

 ;; Execution as standalone test.
 (setf ts1-inst (make-instance (standalone-class-name 'mnst::gr2b 'mnst::ts1)))
 (run ts1-inst)
