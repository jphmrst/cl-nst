;;; File builtins.lisp
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
(in-package :nst-simple-tests)

(def-fixtures f1 ()
  (c 3) (d 'asdfg))

(def-fixtures f1a ()
  (e 'asdfg))

(defclass classcheck ()
     ((s1 :initarg :s1 :reader get-s1)
      (s2 :initarg :s2) (s3 :initarg :s3)))

(def-test-group g1 ()
  (def-test triv :pass))

(def-test-group g1a ()
  (def-test (fix0 :fixtures (f1)) :true (boundp 'c)))

(def-test-group g1a1 ()
  (def-test (fix1 :fixtures (f1)) :true (boundp 'c)))

(def-fixtures f2 (:uses (f1))
  (d 4) (e 'asdfg) (f c))

(def-fixtures f2a (:special (:fixture f1))
  (d 4) (e 'asdfg) (f c))

(def-fixtures fxy ()
  (x 1000) (y 200))

(def-fixtures capture-x-y-fixtures (:assumes (x y))
  (z (+ x y)) (w 10))

(def-fixtures capture-x-y-fixtures-a (:special (x y))
  (z (+ x y)) (w 10))

(def-test-group caps-xy (fxy capture-x-y-fixtures)
  (def-test ex-x (:eql 1000) x)
  (def-test ex-y (:eql  200) y)
  (def-test ex-z (:eql 1200) z)
  (def-test ex-w (:eql   10) w))

(def-test-group caps-xy-a (fxy capture-x-y-fixtures-a)
  (def-test exa-x (:eql 1000) x)
  (def-test exa-y (:eql  200) y)
  (def-test exa-z (:eql 1200) z)
  (def-test exa-w (:eql   10) w))

(def-test-group f2-usage (f1 f2)
  (def-test ex-d (:eql 4) d)
  (def-test ex-e (:eq 'asdfg) e)
  (def-test ex-f (:eql 3) f))

(def-test-group f2a-usage (f1 f2a)
  (def-test exa-d (:eql 4) d)
  (def-test exa-e (:eq 'asdfg) e)
  (def-test exa-f (:eql 3) f))

(def-test-group g2a (f1)
  (def-test using-c :true (boundp 'c)))

(def-test-group g3a (f1)
  (def-test fix1 :true (boundp 'c))
  (def-test fix2 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group g4 (f1)
  (def-test fix1 :true (boundp 'c))
  (def-test fix2 :true (not (boundp '*this-name-should-not-be-bound*))))

(def-test-group h1 (f1 f1a)
  (def-test two-fixtures :true (eq d e))
  )

(defparameter zzz 0)

(nst:def-criterion-alias (-positive-odd)
  "Check whether a number is both positive and odd."
  `(:all (:predicate (lambda (x) (> x 0)))
         (:apply (lambda (x) (mod x 2)) (:eql 1))))

(def-test-group eql-g ()
  (def-test (eql-1) (:eql 3) (+ 2 1)))

(defparameter its-a-list '(a s d f))

(def-test-group core-checks ()
  (def-test oddpos-1 -positive-odd 3)
  (def-test oddpos-2 (:not -positive-odd) 4)
  (def-test pass-1 :pass 'a)
  (def-test fail-1 (:not :fail) 'a)
  (def-test warn-1 (:warn) 3 (+ 1 3))
  (def-test all-1 (:all (:pass) (:warn)) 3 (+ 1 3))
  (def-test (eq-1) (:eq 'ert) 'ert)
  (def-test info-1 (:info "Info note" (:eq 'ertz)) 'ertz)
  (def-test (eql-1) (:eql 3) (+ 2 1))
  (def-test symbol-1 (:symbol ert) 'ert)
  (def-test symbol-2-fails (:not (:symbol ert)) 'erx)
  (def-test apply-1 (:apply length (:predicate numberp)) '(a s d f))
  (def-test apply-2 (:apply length (:predicate numberp)) its-a-list)
  (def-test pred-1 (:predicate numberp) 3)
  (def-test pred-2 (:predicate eql) (+ 1 2) 3)
  (def-test forms-eq-1 :forms-eq (car '(a b c)) (cadr '(c a b)))
  (def-test forms-eql-1 :forms-eql (+ 1 2) 3)
  (def-test forms-equal-1 :forms-equal (mapcar #'1+ '(1 10 100)) '(2 11 101))
  (def-test err-1 :err (error "Catch this error"))
  #-ecl (def-test err-2 (:err :type division-by-zero) (/ 5 0))
  (def-test any-1 (:any (:eql 0) (:eql 1) (:eql 2)) 1)
  (def-test any-2 (:not (:any (:eql 0) (:eql 1) (:eql 2))) 3)
  (def-test any-3 (:any (:err) (:eql 1) (:eql 2)) 1)
  (def-test perf-1 (:perf :sec 3) (sleep 1))
  (def-test perf-2 (:not (:perf :sec 1)) (sleep 3))
  (def-test progn-1 (:progn (setf zzz 2) (:eql 2)) zzz)
  (def-test progn-2 (:progn (setf zzz 3) (setf zzz 2) (:eql 2)) zzz)
  (def-test each1 (:each (:symbol a)) '(a a a a a))
  (def-test seq-1 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b))
  (def-test permute1 (:permute (:each (:eq 'a))) '(a a))
  (def-test permute2
      (:permute (:seq (:symbol b)
                      (:predicate symbolp) (:predicate numberp)))
    '(1 a b))
  (def-test permute2a (:permute (:seq (:eq 'a) (:eq 'b))) '(a b))
  (def-test permute2b (:permute (:seq (:eq 'b) (:eq 'a))) '(a b))
  (def-test no-values1 (:drop-values (:symbol a)) (values 'a 'b 'c))
  (def-test values-drop1 (:apply (lambda (x y) (declare (ignorable y)) x)
                                 (:symbol a)) (values 'a 'b))
  (def-test values-drop3 (:apply (lambda (x y z) (declare (ignorable x z)) y)
                                 (:symbol b))
    (values 'a 'b 'c))
  (def-test value-list1 (:value-list (:seq (:symbol a) (:eq 'b)))
    (values 'a 'b))
  (def-test values1 (:values (:symbol a) (:eq 'b)) (values 'a 'b))
  (def-test across-1 (:across (:symbol a) (:eql 2) (:eq 'b))
    (vector 'a 2 'b))
  (def-test slot0
      (:slots (s1 (:eql 10))
              (s2 (:symbol zz)))
    (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-test slot1
      (:slots (s1 (:eql 10))
              (s2 (:symbol zz))
              (s3 (:seq (:symbol q) (:symbol w)
                        (:symbol e) (:symbol r))))
    (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-test check-err1 (:check-err :forms-eq)
    'asdfgh (error "this should be caught"))
  (def-test check-err2 (:not (:check-err :forms-eq))
    'asdfgh 'asdasdasd)
  (def-test proj-1 (:proj (0 2) :forms-eq) 'a 3 (car '(a b)))
  (def-test (two-fixtures-2 :fixtures (f1 f1a)) :forms-eq d e)
  (def-test sample-1
      (:sample :sample-size 10
               :domains ((x (list :elem symbol)))
               :verify (equal x (reverse (reverse x)))))
  ;;(def-test sample-2
  ;;    (:sample :domains ((x real))
  ;;             :where (> x 1)
  ;;             :verify (< (sqrt x) x)
  ;;             :sample-size 10
  ;;             :max-tries 12))
  (def-test sample-3
      (:sample :domains ((x integer) (y integer))
               :where (> x 0)
               :where-ignore (y)
               :verify (< y (+ y x))
               :sample-size 10
               :max-tries 12))
  (def-test process-1
      (:process (:eval (setf zzz 0))
                (:check (:true-form (eql zzz 0)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 1)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 2)))))
  (def-test eval-1 :eval
    (let ((zzz 'z))
      (assert-eq 'z zzz)))
  (def-test eval-2 :eval
    (let ((zzz 'z))
      (assert-criterion () (:eq 'z) zzz)))
  (def-eval-test eval-3
    (let ((zzz 'z))
      (assert-eq 'z zzz)))
  (def-eval-test eval-4
    (let ((zzz 'z))
      (assert-criterion () (:eq 'z) zzz)))
  (def-test criterion-head-1 (:applying-common-criterion :eq
                               (('a) ('a))
                               (('b) ('b))))
  (def-test criterion-head-2 (:applying-common-criterion :seq
                               (((:eql 'a) (:eql 'b)) ('(a b)))
                               (((:eql 'c) (:eql 'd)) ('(c d)))))
  (def-test criterion-head-3 (:applying-common-criterion :permute
                               (((:seq (:eql 'a) (:eql 'b))) ('(b a)))
                               (((:seq (:eql 'c) (:eql 'd))) ('(c d)))))
  (def-test data-sets-1 (:with-common-criterion (:eq 'a)
                          ('a) ('a)))
  (def-test data-sets-2 (:with-common-criterion (:seq (:eql 'a) (:eql 'b))
                          ('(a b))
                          ((reverse '(b a)))))
  (def-test data-sets-3 (:with-common-criterion (:permute (:seq (:eql 'a)
                                                                (:eql 'b)))
                          ('(b a)) ('(a b)))))

(defparameter for-setup 0
  "This variable will be set by the setup-cleanup tests")

(def-test-group z-setup-cleanup ()
  (def-test z-verify-setup-cleanup (:eql 0) for-setup))

(def-test-group setup-cleanup ()
  (:setup (setf for-setup 1))
  (:cleanup (setf for-setup 0))
  (def-test a-sc-for-setup-1 (:eql 1) for-setup)
  (def-test (sc-for-setup-2 :setup (setf for-setup 2)
                            :cleanup (setf for-setup 1))
      (:eql 2)
    for-setup)
  (def-test z-sc-for-setup-1 (:eql 1) for-setup))

(def-test-group each-setup-cleanup ()
  (:setup (setf for-setup 1))
  (:cleanup (setf for-setup 0))
  (:each-setup (setf for-setup 2))
  (:each-cleanup (setf for-setup 0))
  (def-test a-sc-for-setup-2 (:eql 2) for-setup)
  (def-test (sc-for-setup-3 :setup (setf for-setup 3)
                            :cleanup (setf for-setup 2))
      (:info "This is a known bug" (:eql 3))
    for-setup)
  (def-test z-sc-for-setup-2 (:info "This is a known bug" (:eql 2))
    for-setup)
)

(def-test-group a-setup-cleanup ()
  (def-test a-verify-setup-cleanup (:eql 0) for-setup))

(def-test-group test-adder ()
  (def-test given1 (:each (:symbol a)) '(a a a a a))
  (def-test given2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))

(def-test (given3 :group test-adder) (:predicate numberp) 3)
(def-test (given4 :group test-adder) (:proj (0 2) :forms-eq) 'a 3 (car '(a b)))

(def-test-group includer ()
  (:include-groups included)
  (def-test ib1 (:each (:symbol a)) '(a a a a a))
  (def-test ib2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))

(def-test-group included ()
  (def-test ig1 (:each (:symbol a)) '(a a a a a))
  (def-test ig2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))
