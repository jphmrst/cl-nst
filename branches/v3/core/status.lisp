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

;;;
;;; Result records for high-level checks.
;;;

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
  (check-name *nst-check-name*)
  (warnings nil) (failures nil) (errors nil) (info nil))

(defstruct check-note
  "A single note issued in criteria checking.
 context - the surrounding criteria structure, a list of context-layer structs
 stack - the stack of values at the note point
 format, args - further details; this string and list may e.g. be provided to
                cl:format"
  context stack format args)

(defstruct (error-check-note (:include check-note))
  "A note issued in regards to a thrown error."
  error)

(defstruct (context-layer (:type vector) :named)
  "A record of test criterion
 criterion - the criterion symbol itself
 criterion-args - arguments to the criterion
 given-stack - the stack of values assessed by the criterion"
  criterion criterion-args given-stack)

;;;
;;; Functions on result and status reports.
;;;

(defgeneric result-summary (report &optional
				   code total passed erred failed warned other)
  (:documentation "Receives a reporting structure (or list of them); returns a
six-value summary of the results:
 - A symbol, one of: :error :fail :warn :info :clear
 - The total number of named checks.
 - The number passed.
 - The number raising an error.
 - The number failing.
 - The number giving a warning.")
  
  (:method ((rs null) &optional (code :clear) (total 0) (passed 0)
	                        (erred 0) (failed 0) (warned 0) (other nil))
     (unless other (return-from result-summary
		     (values code total passed erred failed warned)))
     (result-summary other code total passed erred failed warned))
  
  (:method ((rs cons) &optional (code :clear) (total 0) (passed 0)
				(erred 0) (failed 0) (warned 0) (other nil))
     (result-summary (car rs) code total passed erred failed warned
		     (append (cdr rs) other)))
  
  (:method ((r package-result) &optional
	    (code :clear) (total 0) (passed 0) (erred 0) (failed 0)
	    (warned 0) (other nil))
     (result-summary (loop for c being the hash-values
			   of (package-result-group-results r) collect c)
		     code total passed erred failed warned other))
  
  (:method ((r group-result)
	    &optional (code :clear) (total 0) (passed 0)
	    (erred 0) (failed 0) (warned 0) (other nil))
     (result-summary (loop for c being the hash-values
			   of (group-result-check-results r) collect c)
		     code total passed erred failed warned other))
  
  (:method ((r check-result)
	    &optional (code :clear) (total 0) (passed 0)
	    (erred 0) (failed 0) (warned 0) (other nil))
      (with-accessors ((warnings check-result-warnings)
		       (failures check-result-failures)
		       (errors check-result-errors)
		       (info check-result-info)) r
	(result-summary other
			(let ((code1 (cond (errors   :error) (failures :fail)
					   (warnings :warn)  (info     :info)
					   (t :clear))))
			  (case code1
			    (:error code1)
			    (:fail (case code (:error code) (otherwise code1)))
			    (:warn (case code
				     ((:error :fail) code) (otherwise code1)))
			    (:info (case code (:clear code1) (otherwise code)))
			    (:clear code)))
		(+ total 1)
		(+ passed (if (or errors failures) 0 1))
		(+ erred (if errors 1 0))
		(+ failed (if failures 1 0))
		(+ warned (if warnings 1 0))))))

(defun mix-status (&rest stats))

(defmacro count-nonnulls (&rest bools)
  (let ((b (gensym)))
    `(loop for ,b in ,bools sum (if ,b 1 0))))

;;;
;;; Generating status data within checks.
;;;

(defparameter *nst-context* nil
  "Dynamic-scoped variable recording the criteria under test - a list of
context-layer instances.")
(defparameter *nst-stack* nil
  "Dynamic-scoped variable - the stack of values under test by the
current criterion.")
#+allegro(declaim (dynamic-extent *nst-context* *nst-stack*))

(defun emit-warning (&key format args)
  "For use within user-defined check criteria: emit a warning."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (make-check-result
   :warnings (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
(defun emit-failure (&key format args info)
  "For use within user-defined check criteria: explain a failure."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (make-check-result
   :failures (list (make-check-note :context *nst-context* :stack *nst-stack* 
				    :format format :args args))
   :info info))

;;;
;;; Build reports after test runs.
;;;

(defun package-report (&optional (package *package*))
  "Top-level function for reporting the results of a package."
  (let* ((result (make-package-result))
	 (user-package (find-package package))
	 (sym-pack (groups-package user-package)))
    (with-accessors ((name package-result-package-name)
		     (checks package-result-group-results)) result
      (setf name (package-name user-package))
      (do-symbols (remote-group sym-pack)
	(let ((local-group (intern (symbol-name remote-group) user-package)))
	  (setf (gethash local-group checks)
		(group-report local-group)))))
    result))

(defun group-report (group)
  "Top-level function for reporting the results of a group."
  (let ((result (make-group-result)))
    (with-accessors ((name group-result-group-name)
		     (checks group-result-check-results)) result
      (setf name group)
      (loop for test in (test-names group) do
	(setf (gethash test checks)
	      (test-report group test))))
    result))

(defun test-report (group test)
  "Top-level function for reporting the results of a test."
  (gethash (canonical-storage-name (standalone-class-name group test))
	   +results-record+))

;;;
;;; Formatters for reports.
;;;

(defparameter *nst-report-driver* nil
  "Control parameter for building report structures.  Should not be reset from
nil at the top level; set via dynamically-scoped bindings.")

(set-pprint-dispatch 'package-result
  #'(lambda (s pr) 
      (with-accessors ((name package-result-package-name)
		       (checks package-result-group-results)) pr
	(multiple-value-bind (code total passed erred failed warned)
	    (result-summary pr)
	  (declare (ignorable code erred failed warned))
	  (let ((groups
		 (loop for group being the hash-keys of checks collect group)))
	    (pprint-logical-block (s groups)
	      (format s "Package ~a: ~d of ~d passed" name passed total)
	      (loop while (not (pprint-exit-if-list-exhausted)) do
		    (pprint-newline :mandatory s)
		    (let ((name (pprint-pop)))
		      (format s " - ~@<~w~:>" (gethash name checks))))))))))

(set-pprint-dispatch 'group-result
  #'(lambda (s gr) 
      (with-accessors ((name group-result-group-name)
		       (checks group-result-check-results)) gr
	(multiple-value-bind (code total passed erred failed warned)
	    (result-summary gr)
	  (declare (ignorable erred failed warned))
	  (let ((tests
		 (loop for check being the hash-keys of checks collect check)))
	    (pprint-logical-block (s tests)
	      (format s "Group ~a: ~d of ~d passed" name passed total)
	      (case code
		(:clear nil)
		(otherwise
		 (loop while (not (pprint-exit-if-list-exhausted)) do
		       (let* ((name (pprint-pop))
			      (cr (gethash name checks)))
			 (unless (or (eq :clear (result-summary cr))
				     (eq :info (result-summary cr)))
			   (pprint-newline :mandatory s)
			   (format s " - ~w" cr))))))))))))

(set-pprint-dispatch 'check-result
  #'(lambda (s cr) 
      (with-accessors ((check-name check-result-check-name)
		       (warnings check-result-warnings)
		       (failures check-result-failures)
		       (errors check-result-errors)
		       (info check-result-info)) cr
	(let ((total-items (+ (length warnings) (length failures)
			      (length errors)))
	      (succeeded (eql 0 (+ (length failures) (length errors)))))
	  (cond
	   ;; The first three cases are for when we have only one
	   ;; item to report.  We do this on one line if it fits, and
	   ;; don't bother with bullet points.
	   ;;
	   ((and (eql 1 total-items) errors)
	    (format s "~@<Check ~a raised an error:~{~:@_ . ~w~}~:>"
	      check-name errors))

	   ((and (eql 1 total-items) warnings)
	    (format s "~@<Check ~a succeeded with warning~p:~{~:@_ - ~w~}~:>"
	      check-name warnings warnings))

	   ((and (eql 1 total-items) failures)
	    (format s "Check ~a failed" check-name))

	   ;; If we're reporting results for a package or group,
	   ;; suppress the info fields of the report.
	   ;;
	   ((member *nst-report-driver* '(:package :group))
	    (format s "Check ~a: ~@<~@{~:[~2*~;~a~:@_~{ - ~w~^~:@_~}~]~}~:>"
	      check-name
	      errors "Errors:" errors  failures "Failures:" failures
	      warnings "Warnings:" warnings))

	   ;; The default case is (intended to be) for multi-point
	   ;; queries about a specific test.
	   ;;
	   (t (format s "Check ~a ~:[failed~;passed~]: ~
                         ~@<~@{~:[~2*~;~a~:@_~{ - ~w~^~:@_~}~]~}~:>"
		check-name succeeded
		errors "Errors:" errors  failures "Failures:" failures
		warnings "Warnings:" warnings
		info "Additional information:" info)))))))

(set-pprint-dispatch 'check-note
  #'(lambda (s cn) 
      (with-accessors ((context check-note-context)
		       (stack check-note-stack)
		       (format check-note-format)
		       (args check-note-args)) cn
	(declare (ignorable context stack))
	
	(when format (format s "~?" format args))
	(format s "~@<~:[~2*~;~?~:@_~]in context: ~w~:@_stack: ~w~:>"
	  format format args context stack)
	)))

(set-pprint-dispatch 'error-check-note
  #'(lambda (s cn) 
      (with-accessors ((context check-note-context)
		       (stack check-note-stack)
		       (format check-note-format)
		       (args check-note-args)
		       (error error-check-note-args)) cn
	(declare (ignorable context stack))
	(format s "~@<~w~:[~2*~;~:@_~?~]~
                        ~:@_~:[nil context~;~:*in context: ~w~]~
                        ~:@_~:[nil values~;~:*values: ~w~]~:>"
	  error format format args context stack))))

;;;
;;; Report printers.
;;;

(defun report-package (&optional (package *package*)
				 (stream *nst-output-stream*)
				 (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a package."
  (let ((*nst-report-driver* :package))
    (format stream "~w" (package-report package))))

(defun report-group (group &optional (stream *nst-output-stream*)
			   (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a group."
  (let ((*nst-report-driver* :group))
    (format stream "~w" (group-report group))))

(defun report-test (group test &optional (stream *nst-output-stream*)
			       (*nst-local-verbosity* (get-verbosity-level)))
  "Top-level function for reporting the results of a test."
  (let ((*nst-report-driver* :test))
    (format stream "~w" (test-report group test))))
