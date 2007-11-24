;;; File status.lisp
;;;
;;; NST by John Maraist, based on RRT by Robert Goldman.
;;;
;;; NST is Copyright (c) 2006, 2007 Smart Information Flow Technologies.
;;; RRT is Copyright (c) 2005 Robert Goldman, released under the LGPL,
;;; and the lisp-specific preamble to that license.
(in-package :sift.nst)

;;; Result records for high-level checks.

(defstruct (check-result (:type vector) :named)
  (warnings nil) (failures nil) (errors nil) (info nil))

(defstruct (check-note (:type vector) :named)
  context stack format args)

(defstruct (context-layer (:type vector) :named)
  criterion criterion-args given-stack)

(defparameter *nst-context* nil)
(defparameter *nst-stack* nil)
(declaim (dynamic-extent *nst-context* *nst-stack*))

(defun emit-warning (format &rest args)
  (declare (special *nst-context* *nst-stack*))
  (make-check-result
   :warnings (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
(defun emit-failure (format &rest args)
  (declare (special *nst-context* *nst-stack*))
  (make-check-result
   :failures (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
