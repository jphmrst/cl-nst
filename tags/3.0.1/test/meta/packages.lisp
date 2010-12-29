
(defpackage :nst-meta
    (:documentation "Package for reflective NST test suites")
    (:nicknames :mnst)
    (:use :common-lisp :nst :nst-control-api :nst-test-utils)
    (:import-from :nst #:ensure-group-instance #:ensure-test-instance))

(defpackage :nst-meta-sources
    (:documentation
     "Package for the source test suites for reflective NST tests")
    (:nicknames :mnst-src)
    (:use :common-lisp :nst :nst-test-utils))

(defpackage :nst-meta-sources-1
    (:documentation
     "Additional package for the source test suites for reflective NST tests")
    (:nicknames :mnst-src-1)
    (:use :common-lisp :nst :nst-test-utils))

(defpackage :nst-methods-meta-sources
    (:documentation
     "Package for the source test suites for reflective NST tests")
    (:nicknames :mnstmeth-src)
    (:use :common-lisp :nst :nst-test-utils))

