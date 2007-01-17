;;; File test.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :nst)

(defclass classcheck ()
     ((s1 :initarg :s1 :reader get-s1)
      (s2 :initarg :s2) (s3 :initarg :s3)))


(def-fixtures f1 :bindings ((c 3) (d 'asdfg)))
(defmacro result-from-macro () nil)
(def-test-group g1 (f1)
  (def-test t1 :form (eql 1 1))
  (def-test t2 :form (eql 1 2))
  (def-test t3 :form (error "I give an error"))
  (def-test t4 :form (eql c 4))
  (def-test t5 :form (eq d 'asdfg))
  (def-test t6 :form (result-from-macro) :defer-compile t))
(def-fixtures f2 :uses (f1) :bindings ((d 4) (e 'asdfg) (f c)))
(def-test-group g2 (f1 f2)
  (def-check sym1 symbol a (car '(a b c)))
  (def-check not1 not symbol b 'a)
  (def-check sym2 symbol a (car '(a b c)))
  (def-check eq1 eq 'b (cadr '(a b c)))
  (def-check eql1 eql 2 (cadr '(1 2 3)))
  (def-check eqforms1 forms-eq (cadr '(a b c)) (caddr '(a c b)))
  (def-check pred1 predicate numberp 3)
  (def-check err1 err (error "this should be caught"))
  (def-check each1 each symbol a '(a a a a a))
  (def-check not2 not predicate numberp 'a)
  (def-check each2 each predicate numberp '(1 2 3 4 5))
  (def-check seq1
      seq (predicate symbolp) (eql 1) (symbol d)
      '(a 1 d))
  (def-check across1
      across (predicate symbolp) (eql 1)
      (vector 'a 1))
  (def-check permute1 permute each eq 'a '(a a))
  (def-check permute2
      permute seq (symbol b) (predicate symbolp) (predicate numberp)
      '(1 a b))
  (def-check permute3
      not permute seq
      (predicate listp) (predicate symbolp) (predicate numberp)
      '(1 2 3))
  (def-check slot1
      slots (s1 (eql 10)) (s2 (symbol zz))
      (s3 (seq (symbol q) (symbol w) (symbol e) (symbol r)))
      (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-check apply1 apply get-s1 eql 10
    (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-check apply2 apply cadr eql 10 '(0 10 20))
  )

;;(defmacro result-from-macro () t)

