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

(defstruct check-note
  context stack format args)

(defstruct (context-layer (:type vector) :named)
  criterion criterion-args given-stack)

(defparameter *nst-context* nil)
(defparameter *nst-stack* nil)
#+allegro(declaim (dynamic-extent *nst-context* *nst-stack*))

(defun emit-warning (&key format args)
  (declare (special *nst-context* *nst-stack*))
  (make-check-result
   :warnings (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
(defun emit-failure (&key format args info)
  (declare (special *nst-context* *nst-stack*))
  (make-check-result
   :failures (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))
   :info info))

;;; -----------------------------------------------------------------

(defmacro count-nonnulls (&rest bools)
  (let ((b (gensym)))
    `(loop for ,b in ,bools sum (if ,b 1 0))))

(defun format-check-result (s cr &optional c a &rest z)
  (declare (ignorable c a z))
  (with-accessors ((warnings check-result-warnings)
		   (failures check-result-failures)
		   (errors check-result-errors)
		   (info check-result-info)) cr
    (format s "~@<~@{~:[~2*~;~a~:@_~
                  ~{ - ~@<~/nst::format-check-note/~:>~}~:@_~
                    ~]~}~:>"
      errors "Errors:" errors
      failures "Failures:" failures
      warnings "Warnings:" warnings
      info "Info:" info)))

(defun format-check-note (s cn &optional c a &rest z)
  (declare (ignorable c a z))
  (with-accessors ((context check-note-context)
		   (stack check-note-stack)
		   (format check-note-format)
		   (args check-note-args)) cn
    (declare (ignorable context stack))
    (format s "~?" format args)))

