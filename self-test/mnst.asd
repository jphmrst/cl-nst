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
    :nst-groups ((:mnst-simple . g1)
                 (:mnst-simple . g1a)
                 (:mnst-simple . g1a1)
                 (:mnst-simple . g2a)
                 (:mnst-simple . g3a)
                 (:mnst-simple . g4)
                 (:mnst-simple . h1)
                 (:mnst-simple . core-checks)
                 (:mnst-simple . a-setup-cleanup)
                 (:mnst-simple . setup-cleanup)
                 (:mnst-simple . each-setup-cleanup)
                 (:mnst-simple . z-setup-cleanup))
    :depends-on (:nst)
    :components ((:module "core" :components
                          (;; Manually-run tests, for inspecting the
                           ;; order of fixture, setup, cleanup and
                           ;; test execution.
                           (:file "byhand")

                           ;; A simple test suite
                           (:file "builtin-checks")

;;;                       ;; Checks with anonymous fixtures
;;;                       (:file "anon-fixtures-mnst")

                           ))))
