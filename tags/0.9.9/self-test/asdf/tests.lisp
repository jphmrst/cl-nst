;;; File tests.lisp
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
(defpackage :asdf-nst-test
    (:documentation "Package for an ASDF-configured NST test suite")
    (:use :common-lisp :nst))

(in-package :asdf-nst-test)

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
  (def-check values-drop1 (:apply (lambda (x y) x) (:symbol a)) (values 'a 'b))
  (def-check values-drop3 (:apply (lambda (x y z) y) (:symbol b))
    (values 'a 'b 'c))
  (def-check value-list1 (:value-list (:seq (:symbol a) (:eq 'b)))
    (values 'a 'b))
  (def-check values1 (:values (:symbol a) (:eq 'b)) (values 'a 'b))
  (def-check permute2
      (:permute (:seq (:symbol b)
		      (:predicate symbolp) (:predicate numberp)))
    '(1 a b))
  (def-check across-1 (:across (:symbol a) (:eql 2) (:eq 'b))
    (vector 'a 2 'b))
  (def-check check-err1 (:check-err :forms-eq)
    'asdfgh (error "this should be caught"))
  (def-check proj-1 (:proj (0 2) :forms-eq) 'a 3 (car '(a b)))
  )
