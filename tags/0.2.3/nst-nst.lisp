;;; File nst-nst.lisp
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
(in-package :sift.nst-test)

(def-fixtures f1 ()
  (c 3) (d 'asdfg))

(def-fixtures f1a ()
  (e 'asdfg))

(defclass classcheck ()
     ((s1 :initarg :s1 :reader get-s1)
      (s2 :initarg :s2) (s3 :initarg :s3)))

(def-test-group g1 ()
  (def-test triv :form t))

(def-test-group g1a ()
  (def-test fix0 :fixtures (f1)              :form (boundp 'c))
  (def-test fix1 :fixtures ((:fixtures x 3)) :form (boundp 'x)))

(def-test-group g1a1 ()
  (def-test fix0 :fixtures (f1)              :form (boundp 'c)))

(def-test-group g1a2 ()
  (def-test fix1 :fixtures ((:fixtures x 3)) :form (boundp 'x)))

(def-fixtures f2 (:uses (f1))
  (d 4) (e 'asdfg) (f c))

(def-fixtures capture-x-y-fixtures (:assumes (x y))
  (z (+ x y)) (w 10))

(def-test-group g2a (f1)
  (def-test using-c :form (boundp 'c)))

(def-test-group group-with-anon-fixture ((:fixtures zz 3))
  (def-test use-anon-bound :form (boundp 'zz)))

(def-test-group g3 (f1 (:fixtures zz 3 yy 5))
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (boundp 'zz))
  (def-test fix2 :form (eql yy 5)))

(def-test-group g3a (f1)
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group g4 (f1)
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group h1 (f1 f1a)
  (def-test two-fixtures :form (eq d e)))

(def-test-group h2 ()
  (def-test two-fixtures :fixtures (f1 f1a) :form (eq d e)))

