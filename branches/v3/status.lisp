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
  "Overall result structure, containing notes of four distinct types.  A note is
an instance of the check-note structure below.  The four note types are:
 warnings - generated warnings
 failures - criteria which are not met, but not a Lisp error
 errors - Lisp errors
 info - supplimentary information
Each of these fields is a list; warnings, failures and errors are check-note
instances, and the info field is of any value."
  (warnings nil) (failures nil) (errors nil) (info nil))

(defstruct check-note
  "A single note issued in criteria checking.
 context - the surrounding criteria structure, a list of context-layer structs
 stack - the stack of values at the note point
 format, args - further details; this string and list may e.g. be provided to
                cl:format"
  context stack format args)

(defstruct (context-layer (:type vector) :named)
  "A record of test criterion
 criterion - the criterion symbol itself
 criterion-args - arguments to the criterion
 given-stack - the stack of values assessed by the criterion"
  criterion criterion-args given-stack)

(defparameter *nst-context* nil
  "Dynamic-scoped variable recording the criteria under test - a list of
context-layer instances.")
(defparameter *nst-stack* nil
  "Dynamic-scoped variable - the stack of values under test by the
current criterion.")
#+allegro(declaim (dynamic-extent *nst-context* *nst-stack*))

(defun emit-warning (&key format args)
  "For use within user-defined check criteria: emit a warning."
  (declare (special *nst-context* *nst-stack*))
  (make-check-result
   :warnings (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
(defun emit-failure (&key format args info)
  "For use within user-defined check criteria: explain a failure."
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

(set-pprint-dispatch 'check-result
  #'(lambda (s cr) 
      (with-accessors ((warnings check-result-warnings)
		       (failures check-result-failures)
		       (errors check-result-errors)
		       (info check-result-info)) cr
	(format s "~@<~@{~:[~2*~;~a~:@_~{ - ~w~}~:@_~]~}~:>"
	  errors "Errors:" errors
	  failures "Failures:" failures
	  warnings "Warnings:" warnings
	  info "Info:" info))))

(set-pprint-dispatch 'check-note
  #'(lambda (s cn) 
      (with-accessors ((context check-note-context)
		       (stack check-note-stack)
		       (format check-note-format)
		       (args check-note-args)) cn
	(declare (ignorable context stack))
	(format s "~?" format args))))
