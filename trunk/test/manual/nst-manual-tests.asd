;;; File nst-manual-tests.asd
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
(defpackage :nst-manual-asd (:use :common-lisp :asdf))
(in-package :nst-manual-asd)

(defsystem :nst-manual-tests
    :description "Utilities for hand-run NST tests."
    :serial t
    :depends-on (:nst :nst-selftest-utils)
    :components ((:file "package")

                 ;; Manually-run tests, for inspecting the order of
                 ;; fixture, setup, cleanup and test execution.
                 (:file "byhand")))
