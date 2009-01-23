;;; File tests.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

(defparameter o-id-actual nil)

(defmacro outer (o-id &rest forms)
  (let ((o-id-actual o-id))
    (declare (dynamic-extent o-id-actual) (ignorable o-id-actual))
    (let ((actual-inner (loop for form in forms
			      collect (macroexpand form))))
      `'(,@actual-inner))))

(defmacro inner (i-id)
  (declare (special o-id-actual))
  `'(,o-id-actual ,i-id))
