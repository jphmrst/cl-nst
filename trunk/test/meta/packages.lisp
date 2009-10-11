
(defpackage :nst-meta
    (:documentation "Package for reflective NST test suites")
    (:nicknames :mnst)
    (:use :common-lisp :nst))

(defpackage :nst-meta-sources
    (:documentation
     "Package for the source test suites for reflective NST tests")
    (:nicknames :mnst-src)
    (:use :common-lisp :nst))

