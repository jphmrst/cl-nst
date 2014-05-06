;;; File faildata.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Written by John Maraist.
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

(in-package :mnst-src)

(def-test-group simple-pass ()
  (def-test sp :true t))

(def-fixtures boom-fix () (x 3) (y (error "I fail")) (z 10))

(def-test-group boom-group-fixture (boom-fix)
  (def-test bf1 :true t)
  (def-test bf2 :true t)
  (def-test bf3 :true t)
  (def-test bf4 :true t))

(def-test-group boom-group-setup ()
  (:setup (error "Setup error"))
  (:cleanup t)
  (def-test bgs1 :true t)
  (def-test bgs2 :true t)
  (def-test bgs3 :true t)
  (def-test bgs4 :true t))

(def-test-group boom-group-cleanup ()
  (:setup t)
  (:cleanup (error "Setup error"))
  (def-test bgc1 :true t)
  (def-test bgc2 :true t)
  (def-test bgc3 :true t)
  (def-test bgc4 :true t))

(def-test-group boom-test-fixture ()
  (def-test bf1 :true t)
  (def-test (bf2 :fixtures (boom-fix)) :true t)
  (def-test bf3 :true t)
  (def-test bf4 :true t))

(def-test-group boom-test-setup ()
  (def-test bts1 :true t)
  (def-test (bts2 :setup (error "Setup error") :cleanup t) :true t)
  (def-test bts3 :true t)
  (def-test bts4 :true t))

(def-test-group boom-test-cleanup ()
  (def-test btc1 :true t)
  (def-test (btc2 :setup t
                  :cleanup (progn ;; (format t "YYYYYYYYYY~%")
                                  (error "Setup error")))
      :true t)
  (def-test btc3 :true t)
  (def-test btc4 :true t))

(def-test-group fail-tests ()
  (def-test ft1 :true t)
  (def-test ft2 :true nil)
  (def-test ft3 :true (null t))
  (def-test ft4 :true t))

(def-test-group miss-difftyped-err ()
  (def-test err-3 (:err :type division-by-zero) (error "Miss this"))
  ;; (def-test err-4 (:eql 1) (div-five-by 0))
  )

(def-test-group simple-checkpoints ()
  (def-test check-pass-1 :pass 'a)
  (def-test check-pass-2 (:eq 'ert) 'ert)
  (def-test check-pass-3 (:eql 3) (+ 2 1))
  (def-test check-pass-4 (:symbol ert) 'ert)
  (def-test check-fail-1 :fail 'a)
  (def-test check-fail-2 (:eql 5) (+ 2 1))
  (def-test check-fail-3 (:symbol tre) 'ert)
  (def-test check-error-1 :true (error "This throws an error")))

(defun div-five-by (x) (/ 5 x))

;; For checking permutations of an empty list.
(def-test-group permute-nil-g ()
  (def-test pass-though-no-cands-0 (:permute :pass) '())
  (def-test fail-for-no-cands-1 (:permute (:seq (:eq 'a))) '())
  (def-test fail-for-no-cands-2 (:permute (:seq (:eq 'a))) '())
  (def-test fail-on-empty (:permute (:seq (:eq 'a) (:eq 'b))) '())
  (def-test fail-on-fewer (:permute (:seq (:eq 'a) (:eq 'b))) '(a))
  (def-test fail-on-more (:permute (:seq (:eq 'a) (:eq 'b))) '(a c e)))

;; A test which fails in one obscure corner of its criterion, so that
;; the failure report has a bit of a stack to it.
(def-test-group deeper-fail-report ()
  (def-test placehold-0 :pass '())
  (def-test deep-fail-1 (:seq :pass
                              (:all :pass (:eql 3) (:symbol b)))
    '(a b))
  (def-test placehold-1 :pass 'a)
  )

(defparameter zzz 0)

(def-fixtures fixture-with-nil ()
  (c 3)
  (nil (setf zzz 10))
  (d 'asdfg))

(def-test-group use-fixture-with-nil (fixture-with-nil)
  (def-test tN1 :true (boundp 'c))
  (def-test tN2 (:eq 'asdfg) d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing include-groups

(def-test-group base-include ()
  (:include-groups included-1 included-2)
  (def-test inc1 (:each (:symbol a)) '(a a a a a))
  (def-test inc2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))

(def-test-group included-1 ()
  (def-test inc1-1 (:each (:symbol a)) '(a a a a a))
  (def-test inc1-2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))

(def-test-group included-2 ()
  (def-test inc2-1 (:each (:symbol a)) '(a a a a a))
  (def-test inc2-2 (:seq (:symbol a) (:eql 2) (:eq 'b)) '(a 2 b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing the number of times a test is executed

(defvar *test-exec-counter* 0)
(def-test-group counter-tests ()
   (def-test counter-test :true
     (incf *test-exec-counter*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Failure cases for the :process criterion

(def-test-group process-failures ()
  (def-test process-fail-1
      (:process (:eval (setf zzz 0))
                (:check (:true-form (eql zzz 1))
                        (:true-form (eql zzz 10)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 2)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 3)))))
  (def-test process-fail-2
      (:process (:eval (setf zzz 0))
                (:check (:true-form (eql zzz 1))
                        (:true-form (eql zzz 10)))
                (:eval (error "Blah"))
                (:check (:true-form (eql zzz 2)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 3)))))
  (def-test process-fail-3
      (:process (:eval (setf zzz 0))
                (:check (:true-form (eql zzz 1))
                        (:true-form (eql zzz 10)))
                (:eval (error "Blah"))
                (:errcheck)
                (:check (:true-form (eql zzz 2)))
                (:eval (incf zzz))
                (:check (:true-form (eql zzz 3))))))

(def-test-group eval-tests ()
  (def-test eval-1 :eval
    (let ((zzz 'z))
      (assert-eq 'z zzz)
      (assert-eq 'z zzz)))
  (def-test eval-2 :eval
    (let ((zzz 'z))
      (assert-eq 'x zzz)
      (assert-eq 'y zzz)))
  (def-test eval-3 (:eval :attempt-continue nil)
    (let ((zzz 'z))
      (assert-eq 'x zzz)
      (assert-eq 'y zzz)))
  (def-test eval-4 :eval
    (let ((zzz 'z))
      (assert-eq 'x zzz)
      (assert-eq 'z zzz)))
  (def-test eval-5 :eval
    (let ((zzz 'z))
      (assert-eq 'x zzz)))
  (def-test eval-6 :eval
    (let ((zzz (eval '(+ 1 1))))
      (assert-eq 2 zzz)))
  (def-test eval-eql-1 :eval
    (let ((zzz 1))
      (assert-eql 1 zzz)))
  (def-test eval-eql-2 :eval
    (let ((zzz 1))
      (assert-eql 2 zzz)
      (assert-eql 3 zzz)))
  (def-test eval-eql-3 :eval
    (let ((zzz 1))
      (assert-eql 1 zzz)
      (assert-eql 2 zzz)
      (assert-eql 3 zzz)))
  (def-test eval-eql-4 (:eval :attempt-continue nil)
    (let ((zzz 1))
      (assert-eql 1 zzz)
      (assert-eql 2 zzz)
      (assert-eql 3 zzz)))
  (def-test eval-not-eql-1 :eval
    (let ((zzz 1))
      (assert-not-eql 2 zzz)))
  (def-test eval-not-eql-2 :eval
    (let ((zzz 1))
      (assert-not-eql 2 zzz)
      (assert-not-eql 3 zzz)))
  (def-test eval-not-eql-3 :eval
    (let ((zzz 1))
      (assert-not-eql 1 zzz)
      (assert-not-eql 2 zzz)
      (assert-not-eql 3 zzz)
      (assert-not-eql 1 zzz)))
  (def-test eval-not-eql-4 (:eval :attempt-continue nil)
    (let ((zzz 1))
      (assert-not-eql 1 zzz)
      (assert-not-eql 2 zzz)
      (assert-not-eql 3 zzz)
      (assert-not-eql 1 zzz)))
  (def-test eval-not-eql-5 (:eval :attempt-continue nil)
    (let ((zzz 1))
      (assert-not-eql 2 zzz)
      (assert-not-eql 1 zzz)
      (assert-not-eql 3 zzz)
      (assert-not-eql 1 zzz)))

  (def-test eval-criterion-pass :eval
    (let ((zzz 'z))
      (assert-criterion () (:eq 'z) zzz)))
  (def-test eval-criterion-fail :eval
    (let ((zzz 'z))
      (assert-criterion () (:eq 'zz) zzz)))
  (def-test eval-criterion-error :eval
    (let ((zzz 'z))
      (assert-criterion () (:eq 'z) (error "XXX")))))

(def-test-group asp-group-0 ()
  (:aspirational t)
  (def-test ag0-t1 (:eq 'a) 'a)
  (def-test ag0-t2 (:eq 'a) 'b))

(def-test-group asp-group-1 ()
  (:aspirational t)
  (def-test (ag1-t1 :aspirational t) (:eq 'a) 'a)
  (def-test (ag1-t2) (:eq 'a) 'b)
  (def-test (ag1-t3 :aspirational nil) (:eq 'a) 'b))

(def-test-group asp-group-2 ()
  (def-test (ag2-t1 :aspirational t) (:eq 'a) 'a)
  (def-test (ag2-t2) (:eq 'a) 'b)
  (def-test (ag2-t3 :aspirational nil) (:eq 'a) 'b))

(def-test-group asp-group-3 ()
  (def-test ag3-t1 (:eq 'a) 'a)
  (def-test (ag3-t2 :aspirational t) (:eq 'a) 'b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing export of names.

(in-package :mnst-src-1)

(def-fixtures exp-fix-0 ()
  (fix0a :true)
  (fix0b :false))

(def-fixtures exp-fix-1 (:export-names t)
  (fix1a :true)
  (fix1b :false))

(def-fixtures exp-fix-2 (:export-fixture-name t)
  (fix2a :true)
  (fix2b :false))

(def-fixtures exp-fix-3 (:export-bound-names t)
  (fix3a :true)
  (fix3b :false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NB --- search for in-package --- the end of this file is NOT in
;; :mnst-src.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
