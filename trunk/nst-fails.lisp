;;; File defcheck.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow
;;; Technologies.  RRT is Copyright (c) 2005 Robert Goldman, released
;;; under the LGPL, and the lisp-specific preamble to that license.
(in-package :nst-interact)

;;; Some examples of failing tests.


(nst:def-test-group failing1 ()
  (nst:def-check bad-symbol
      (:eq 'r)
    's))

