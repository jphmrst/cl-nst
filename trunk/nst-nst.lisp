;;; File nst-nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst-test)

(def-fixtures f1 :bindings ((c 3) (d 'asdfg)))

(def-test-group nst-tests ()
  (def-check symbol-1 :symbol a 'a)
  (def-check symbol-2 :not :symbol b 'a)  
  (def-check eql-1 :eql 4 (+ 1 3)))

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

(def-fixtures f2 :uses (f1)
	      :bindings ((d 4) (e 'asdfg) (f c)))

(def-fixtures capture-x-y-fixtures :assumes (x y)
	      :bindings ((z (+ x y)) (w 10)))

(def-test-group g2a (f1)
  (def-test using-c :form (boundp 'c)))

(def-test-group g2 (f1 f2)
  (def-check sym1 :symbol a (car '(a b c)))
  (def-check not1 :not :symbol b 'a)
  (def-check sym2 :symbol a (car '(a b c)))
  (def-check eq1 :eq 'b (cadr '(a b c)))
  (def-check eql1 :eql 2 (cadr '(1 2 3)))
  (def-check eqforms1 :forms-eq (cadr '(a b c)) (caddr '(a c b)))
  (def-check pred1 :predicate numberp 3)
  (def-check err1 :err (error "this should be caught"))
  (def-check each1 :each :symbol a '(a a a a a))
  (def-check not2 :not :predicate numberp 'a)
  (def-check each2 :each :predicate numberp '(1 2 3 4 5))
  (def-check seq0 :seq nil)
  (def-check seq0e :not :seq (:eql 1) nil)
  (def-check seq1
      :seq (:predicate symbolp) (:eql 1) (:symbol d)
      '(a 1 d))
  (def-check with-seq1
      :with (:seq (:predicate symbolp) (:eql 1) (:symbol d))
      '(a 1 d))
  (def-check across1
      :across (:predicate symbolp) (:eql 1)
      (vector 'a 1))
  (def-check permute0 :permute :seq nil)
  (def-check permute1 :permute :each :eq 'a '(a a))
  (def-check permute2 :permute
    :seq (:symbol b) (:predicate symbolp) (:predicate numberp)
    '(1 a b))
  (def-check permute3
      :not :permute :seq
      (:predicate listp) (:predicate symbolp) (:predicate numberp)
      '(1 2 3))
  (def-check slot1
      :slots (s1 (:eql 10)) (s2 (:symbol zz))
      (s3 (:seq (:symbol q) (:symbol w) (:symbol e) (:symbol r)))
      (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-check apply1 :apply get-s1 :eql 10
    (make-instance 'classcheck :s1 10 :s2 'zz :s3 '(q w e r)))
  (def-check apply2 :apply cadr :eql 10 '(0 10 20))
  (def-check multi1 :multi
    (:eql 10) (:predicate symbolp) (:predicate stringp)
    (values 10 'a "sss"))

  (def-check float1 :round-sig-eql 3 142.1 141.9)
  (def-check float2 :not :round-sig-eql 3 141.1 141.9)

  (def-test fix0 :fixtures (f1)                  :form (boundp 'c))
  (def-test fix1 :fixtures ((:fixtures x 3))     :form (boundp 'x))
  (def-test fix2 :fixtures (f1 (:fixtures x 3))  :form (eql c x))
  (def-test fix3 :fixtures (f1 (:fixtures qq 3)) :form (eql c qq))
  (def-test fix4 :fixtures (f1 (:fixtures x 3))  :form (eql c 3))

  (def-check checkfix1 :fixtures ((:fixtures qq 5))
	     :eql 5 qq)
  
  (def-check check-capture-0
      :fixtures ((:fixtures x 1 y 2) capture-x-y-fixtures)
      :pass)
  (def-check check-capture-1
      :fixtures ((:fixtures x 1 y 2) capture-x-y-fixtures)
      :eql 3 z)
  (def-check check-capture-2
      :fixtures ((:fixtures x 1 y 2) capture-x-y-fixtures)
      :setup (setf w 100)
      :eql 100 w)
  )

(def-test-group group-with-anon-fixture ((:fixtures zz 3))
  (def-test use-anon-bound :form (boundp 'zz)))

(def-test-group g3 (f1 (:fixtures zz 3 yy 5))
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (boundp 'zz))
  (def-test fix2 :form (eql yy 5)))

(def-test-group g3a (f1)
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (not (boundp 'zz)))
  )

(def-test-group g4 (f1)
  (def-test fix0 :form (boundp 'c))
  (def-test fix1 :form (not (boundp 'zz)))
  )

(def-check-criterion :carcarcdr :args (x y zs)
		     :expansion `(:all (:apply car :with ,x)
				       (:apply cadr :with ,y)
				       (:apply cddr :with ,zs)))

(def-check-criterion :car-fits-any-but-first
    :args (x) :rest rest
    :expansion `(:apply car :any ,@rest))

(def-check-criterion :cc
    :args (c1 c2 c3)
    :expansion `(:slots (s1 ,c1) (s2 ,c2) (s3 ,c3)))

(def-test-group g5 ()
  (def-check ccc1
      :carcarcdr (:eq 'a) (:eql 3) (:apply length :eql 2) '(a 3 2 1))
  (def-check cfabf1
      :car-fits-any-but-first (:eq 'c) (:eq 'a) (:eql 3)
                              (:apply length :eql 2)
      '(a 3 2 1))
  (def-check cfabf2
      :not :car-fits-any-but-first
      (:eq 'a) (:eq 'c) (:eql 3) (:predicate listp)
      '(a 3 2 1))
  (def-check cc-1
      :cc (:eql 1) (:symbol p) (:eq 'd)
      (make-instance 'classcheck :s1 1 :s2 'p :s3 'd)))
