;;; File packages.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

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

