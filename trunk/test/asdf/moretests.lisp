;;; File moretests.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
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
(defpackage :asdf-nst-test2
    (:documentation "Second package for an ASDF-configured NST test suite")
    (:use :common-lisp :nst))

(in-package :asdf-nst-test2)

(def-fixtures fix-for-more () (c 3) (d 'asdfg))

(def-test-group more1 ()
  (def-test triv0 :pass))

(def-test-group more2 ()
  (def-test triv1 :pass)
  (def-test (fix0 :fixtures (fix-for-more)) :true (boundp 'c)))

(def-test-group more3 ()
  (def-test (fix1 :fixtures (fix-for-more)) :true (boundp 'c)))
