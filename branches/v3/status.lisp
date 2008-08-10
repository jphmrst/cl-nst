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

(defstruct package-result
  "Overall package result structure, mapping groups to results by name."
  package-name
  (group-results (make-hash-table :test 'eq)))

(defstruct group-result
  "Overall group result structure, mapping checks to results by name."
  group-name
  (check-results (make-hash-table :test 'eq)))

(defstruct check-result
  "Overall check result structure, containing notes of four distinct types.  A
note is an instance of the check-note structure below.  The four note types are:
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
   :failures (list (make-check-note :context *nst-context* :stack *nst-stack* 
				    :format format :args args))
   :info info))

;;; -----------------------------------------------------------------

(defun package-report (&optional (package *package*))
  "Top-level function for reporting the results of a package."
  (let* ((result (make-package-result))
	 (user-package (find-package package))
	 (sym-pack (groups-package user-package)))
    (with-accessors ((name package-result-package-name)
		     (checks package-result-group-results)) result
      (do-symbols (remote-group sym-pack)
	(let ((local-group (intern (symbol-name remote-group) user-package)))
	  (setf (gethash local-group checks) (group-report local-group)))))
    result))

(defun group-report (group)
  "Top-level function for reporting the results of a group."
  (let ((result (make-group-result)))
    (with-accessors ((name group-result-group-name)
		     (checks group-result-check-results)) result
      (setf name group)
      (loop for test in (test-names group) do
	(setf (gethash test checks) (test-report group test))))
    result))

(defun test-report (group test)
  "Top-level function for reporting the results of a test."
  (gethash (canonical-storage-name (standalone-class-name group test))
	   +results-record+))

;;; -----------------------------------------------------------------

(defun report-package (&optional (package *package*)
				 (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a package."
  (format *nst-output-stream* "~w" (package-report package)))

(defun report-group (group &optional
			   (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a group."
  (format *nst-output-stream* "~w" (group-report group)))

(defun report-test (group test &optional
			       (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a test."
  (format *nst-output-stream* "~w" (test-report group test)))

;;; -----------------------------------------------------------------

(defmacro count-nonnulls (&rest bools)
  (let ((b (gensym)))
    `(loop for ,b in ,bools sum (if ,b 1 0))))

(set-pprint-dispatch 'package-result
  #'(lambda (s pr) 
      (with-accessors ((name package-result-package-name)
		       (checks package-result-group-results)) pr
	(let ((groups
	       (loop for group being the hash-keys of checks collect group)))
	  (pprint-logical-block (s groups)
	    (format s "Package ~a" name)
	    (loop while (not (pprint-exit-if-list-exhausted)) do
	      (pprint-newline :mandatory s)
	      (let ((name (pprint-pop)))
		(format s " - Group ~a: ~@<~w~:>"
		  name (gethash name checks)))))))))

(set-pprint-dispatch 'group-result
  #'(lambda (s gr) 
      (with-accessors ((name group-result-group-name)
		       (checks group-result-check-results)) gr
	(let ((tests
	       (loop for check being the hash-keys of checks collect check)))
	  (pprint-logical-block (s tests)
	    (format s "Group ~a" name)
	    (loop while (not (pprint-exit-if-list-exhausted)) do
	      (pprint-newline :mandatory s)
	      (let ((name (pprint-pop)))
		(format s " - Test ~a: ~@<~w~:>"
		  name (gethash name checks)))))))))

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
