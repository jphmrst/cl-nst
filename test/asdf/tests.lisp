;;; File tests.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
(defpackage :asdf-nst-test
    (:documentation "Package for an ASDF-configured NST test suite")
    (:use :common-lisp :nst))

(in-package :asdf-nst-test)

(def-test-group group1 ()
  (def-test pass-1 :pass 'a)
  (def-test fail-1 (:not :fail) 'a)
  (def-test seq (:seq (:pass) (:eql 4)) (list 3 (+ 1 3))))