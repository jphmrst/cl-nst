;;; File nst-meta-tests.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.

(asdf:oos 'asdf:load-op :asdf-nst)
(defpackage :nst-meta-asd (:use :common-lisp :asdf))
(in-package :nst-meta-asd)

(defsystem :nst-meta-tests
    :class nst-test-holder
    :description "M as in meta: NST- (or otherwise) testing NST."
    :serial t
    :nst-packages (:mnst)
    :depends-on (:nst :nst-selftest-utils)
    :components ((:file "packages")

                 ;; Criteria for NST-testing the result of running
                 ;; NST.
                 (:file "reflect")

                 ;; Source groups and tests about fixture caching.
                 (:file "caching")

                 ;; NST tests expected to produce errors, to be run
                 ;; reflectively to confirm the expected failure
                 ;; states.
                 (:file "faildata")

                 ;; Classes and test methods for meta-tests of the
                 ;; :methods criterion.
                 #-lispworks
                 (:file "methods")

                 ;; NST tests on NST runs.
                 (:file "selftest")

                 ;; Testing redefining tests.
                 (:file "redefs")))
