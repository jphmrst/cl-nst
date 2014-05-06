
;;; File anon-fixtures-mnst.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
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
(defpackage :mnst-anon-fixtures
    (:documentation "Package for a simple NST test suite")
    (:use :common-lisp :nst))

(in-package :mnst-anon-fixtures)

(def-fixtures f1 ()
  (c 3) (d 'asdfg))

(def-fixtures f1a ()
  (e 'asdfg))

(defclass classcheck ()
     ((s1 :initarg :s1 :reader get-s1)
      (s2 :initarg :s2) (s3 :initarg :s3)))

(def-test-group g1 ()
  (def-check triv :pass))

(def-test-group g1a ()
  (def-check (fix0 :fixtures (f1))              :true (boundp 'c))
  (def-check (fix1 :fixtures ((:fixtures x 3))) :true (boundp 'x)))

(def-test-group g1a1 ()
  (def-check (fix0 :fixtures (f1))              :true (boundp 'c)))

(def-test-group g1a2 ()
  (def-check (fix1 :fixtures ((:fixtures x 3))) :true (boundp 'x)))

(def-fixtures f2 (:uses (f1))
  (d 4) (e 'asdfg) (f c))

(def-fixtures capture-x-y-fixtures (:assumes (x y))
  (z (+ x y)) (w 10))

(def-test-group g2a (f1)
  (def-check using-c :true (boundp 'c)))

(def-test-group group-with-anon-fixture ((:fixtures zz 3))
  (def-check use-anon-bound :true (boundp 'zz)))

(def-test-group g3 (f1 (:fixtures zz 3 yy 5))
  (def-check fix0 :true (boundp 'c))
  (def-check fix1 :true (boundp 'zz))
  (def-check fix2 :true (eql yy 5)))

(def-test-group g3a (f1)
  (def-check fix0 :true (boundp 'c))
  (def-check fix1 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group g4 (f1)
  (def-check fix0 :true (boundp 'c))
  (def-check fix1 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group h1 (f1 f1a)
  (def-check two-fixtures :true (eq d e)))

(def-test-group h2 ()
  (def-check (two-fixtures :fixtures (f1 f1a)) :true (eq d e)))

