
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

