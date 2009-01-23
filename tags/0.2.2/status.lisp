;;; File status.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
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

