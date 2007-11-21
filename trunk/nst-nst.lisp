;;; File nst-nst.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
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

