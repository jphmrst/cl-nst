;;; File simple-mnst.lisp
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
(defpackage :mnst-simple
    (:documentation "Package for a simple NST test suite")
    (:use :common-lisp :nst))

(in-package :mnst-simple)

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
  (def-check (fix0 :fixtures (f1)) :true (boundp 'c)))

(def-test-group g1a1 ()
  (def-check (fix0 :fixtures (f1)) :true (boundp 'c)))

(def-fixtures f2 (:uses (f1))
  (d 4) (e 'asdfg) (f c))

(def-fixtures capture-x-y-fixtures (:assumes (x y))
  (z (+ x y)) (w 10))

(def-test-group g2a (f1)
  (def-check using-c :true (boundp 'c)))

(def-test-group g3a (f1)
  (def-check fix0 :true (boundp 'c))
  (def-check fix1 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group g4 (f1)
  (def-check fix0 :true (boundp 'c))
  (def-check fix1 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group h1 (f1 f1a)
;;;  (def-check two-fixtures :true (eq d e))
  )

(defparameter zzz 0)

(def-test-group core-checks ()
  (def-check pass-1 :pass 'a)
  (def-check fail-1 (:not :fail) 'a)  
  (def-check warn-1 (:warn) 3 (+ 1 3))
  (def-check all-1 (:all (:pass) (:warn)) 3 (+ 1 3))
  (def-check (eq-1) (:eq 'ert) 'ert)
  (def-check (eql-1) (:eql 3) (+ 2 1))
  (def-check symbol-1 (:symbol ert) 'ert)
  (def-check symbol-2-fails (:not (:symbol ert)) 'erx)
  (def-check apply-1 (:apply length (:predicate numberp)) '(a s d f))
  (def-check pred-1 (:predicate numberp) 3)
  (def-check pred-2 (:predicate eql) (+ 1 2) 3)
  (def-check forms-eq-1 :forms-eq (car '(a b c)) (cadr '(c a b)))
  (def-check forms-eql-1 :forms-eql (+ 1 2) 3)
  (def-check forms-equal-1 :forms-equal (mapcar #'1+ '(1 10 100)) '(2 11 101))
  (def-check err-1 :err (error "Catch this error"))
  (def-check any-1 (:any (:eql 0) (:eql 1) (:eql 2)) 1)
  (def-check any-2 (:not (:any (:eql 0) (:eql 1) (:eql 2))) 3)
  (def-check any-3 (:any (:err) (:eql 1) (:eql 2)) 1)
  (def-check perf-1 (:perf :sec 3) (sleep 1))
  (def-check perf-2 (:not (:perf :sec 1)) (sleep 3))
  (def-check progn-1 (:progn (setf zzz 2) (:eql 2)) zzz)
  (def-check progn-2 (:progn (setf zzz 3) (setf zzz 2) (:eql 2)) zzz)
  (def-check each1 (:each (:symbol a)) '(a a a a a))
  (def-check seq-1 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b))
  (def-check permute1 (:permute (:each (:eq 'a))) '(a a))
;;;  (def-check values1 (:values (:seq (:eq 'a) (:eq 'b))) (values 'a 'b))
  (def-check permute2
      (:permute (:seq (:symbol b)
		      (:predicate symbolp) (:predicate numberp)))
    '(1 a b))
  (def-check across-1 (:across (:symbol a) (:eql 2) (:eq 'b))
    (vector 'a 2 'b))
  (def-check slot1
      (:slots (s1 (:eql 10))
	      (s2 (:symbol zz))
	      (s3 (:seq (:symbol q) (:symbol w)
			(:symbol e) (:symbol r))))
    (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-check check-err1 (:check-err :forms-eq)
    'asdfgh (error "this should be caught"))
  (def-check proj-1 (:proj (0 2) :forms-eq) 'a 3 (car '(a b)))
;;;  (def-check (two-fixtures :fixtures (f1 f1a)) :forms-eq d e)
  )
