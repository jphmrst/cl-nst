;;; File byhand-mnst.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
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
(defpackage :mnst-manual
    (:documentation "Manually-run tests and utilities for NST")
    (:use :common-lisp :nst))

(in-package :mnst-manual)

;;; This file contains a sample test block under development.

(def-value-check (:echo () (&rest chk))
  `(declare (ignorable chk))
  `(progn
     (format t "              * Core check echo~%")
     (nst::make-check-result)))

(def-fixtures fix1 () (x 3) (y (format t "Bindings from fix1~%")))
(def-fixtures fix2 () (z2 (format t "        Bindings from fix2~%")))
(def-fixtures fix3 () (z3 (format t "Bindings from fix3~%")))

(def-test-group gr0 (fix1))
(def-test-group gr1 (fix1) (def-check t1 :echo ()))
(def-test-group gr2a (fix1) (def-check ts1 :echo ()) (def-check ts2 :echo ()))
(def-test-group gr2b (fix1)
  (:setup   (format t "  Setup for group gr2b~%"))
  (:cleanup (format t "  Cleanup for group gr2b~%"))
  (:each-setup   (format t "          Setup for each test of group gr2b~%"))
  (:each-cleanup (format t "          Cleanup for each test of group gr2b~%"))
  (def-check (ts1 :fixtures (fix2)
                  :setup (format t "            Setup for ts1~%")
                  :cleanup (format t "            Cleanup for ts1~%"))
      :echo ())
  (def-check ts2 :echo ()))



(def-test-group failures ()
  (def-check seq-1 (:seq (:symbol a) (:eql 3) (:eq 'b)) '(a 2 b))
  (def-check seq-2 (:seq (:symbol a) (:eql 3) (:eq 'b)) (error "boom"))
  )

(eval-when (:load-toplevel :execute)
  (defparameter gr1-class (nst::group-class-name 'gr1))
  (defparameter gr1-inst (nst::make-instance gr1-class))
  (defparameter gr2a-class (nst::group-class-name 'gr2a))
  (defparameter gr2a-inst (nst::make-instance gr2a-class))
  (defparameter gr2b-class (nst::group-class-name 'gr2b))
  (defparameter gr2b-inst (nst::make-instance gr2b-class)))
