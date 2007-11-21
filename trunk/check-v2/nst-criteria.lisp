;;; File nst-nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst-test)

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

  (def-check (two-fixtures :fixtures (f1 f1a)) :forms-eq d e))

