;;; File nst-interact.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst-interact)

(def-test-group interact-b ()
  (def-test fix0 :form (eql 1 1))
  (def-test broke :form (eql 1 2))
  (def-test fix1 :form (eql 2 2)))

(def-test-group interact-e ()
  (def-test fix0 :form (eql 1 1))
  (def-test broke :form (error "boom"))
  (def-test fix1 :form (eql 2 2)))
