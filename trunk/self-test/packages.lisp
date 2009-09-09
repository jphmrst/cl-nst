(defpackage :mnst-simple
    (:documentation "Package for a simple NST test suite")
    (:use :common-lisp :nst))

(defpackage :mnst
    (:documentation "Package for reflective NST test suites")
    (:use :common-lisp :nst))

(defpackage :mnst-reflect-src
    (:documentation
     "Package for the source test suites for reflective NST tests")
    (:nicknames :mnst-src)
    (:use :common-lisp :nst))

