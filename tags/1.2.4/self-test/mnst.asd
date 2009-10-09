;;; File mnst.asd
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
(defpackage :mnst-asd (:use :common-lisp :asdf))
(in-package :mnst-asd)

(defsystem :mnst
    :class nst-test-holder
    :description "M as in meta: NST- (or otherwise) testing NST."
    :serial t
    :nst-systems (:masdfnst)
    :nst-packages (:mnst)
    :depends-on (:nst)
    :components ((:file "packages")

                 ;; A simple test suite
                 (:file "builtins")

                 ;; Manually-run tests, for inspecting the order of
                 ;; fixture, setup, cleanup and test execution.
                 (:file "byhand")

                 ;; Criteria for NST-testing the result of running
                 ;; NST.
                 (:file "reflect")

                 ;; NST tests expected to produce errors, to be run
                 ;; reflectively to confirm the expected failure
                 ;; states
                 (:file "faildata")

                 ;; NST tests on NST runs.
                 (:file "selftest")

;;;              ;; Checks with anonymous fixtures
;;;              (:file "anon-fixtures-mnst")

                 ))
