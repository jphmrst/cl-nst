;;; File status.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
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
  (check-result
   :warnings (list (make-check-note :context *nst-context*
				    :stack *nst-stack*
				    :format format :args args))))
(defun emit-failure (&key format args info)
  "For use within user-defined check criteria: explain a failure."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (check-result
   :failures (list (make-check-note :context *nst-context* :stack *nst-stack* 
				    :format format :args args))
   :info info))
(defun emit-success ()
  "For use within user-defined check criteria: record a successful check."
  (check-result))

;;;
;;; Result records for high-level checks.
;;;

(defstruct (multi-results (:include result-stats))
  "Multiple results structure."
  package-reports group-reports test-reports system)

(set-pprint-dispatch 'multi-results
  #'(lambda (s res)
      (with-accessors ((packages multi-results-package-reports)
		       (groups multi-results-group-reports)
		       (tests multi-results-test-reports)
		       (system multi-results-system)) res
	
	(when system
	  (format s "~%Summary of results for system ~a:~%"
	    (slot-value system 'asdf::name)))
	(let ((reports
	       (nconc (loop for report in packages
			    collect (let ((*nst-report-driver* :package))
				      (declare (special *nst-report-driver*))
				      (format s "~w~%" report)
				      report))
		      (loop for report in groups
			    collect (let ((*nst-report-driver* :group))
				      (declare (special *nst-report-driver*))
				      (format s "~w~%" report)
				      report))
		      (loop for report in tests
			    collect (let ((*nst-report-driver* :test))
				      (declare (special *nst-report-driver*))
				      (format s "~w~%" report)
				      report)))))
	  (multiple-value-bind (code total passed erred failed warned)
	      (result-summary reports)
	    (declare (ignorable code))
	    (format s
		"TOTAL: ~d of ~d passed (~d failed, ~d error~p, ~d warning~p)~%"
	      passed total failed erred erred warned warned))))))

(defstruct (package-result (:include result-stats))
  "Overall package result structure, mapping groups to results by name."
  package-name
  (group-results (make-hash-table :test 'eq)))

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

(defstruct (group-result (:include result-stats))
  "Overall group result structure, mapping checks to results by name."
  group-name
  (check-results (make-hash-table :test 'eq)))

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

(defstruct (check-result (:include result-stats (tests 1)))
  "Overall check result structure, containing notes of four distinct types.  A
note is an instance of the check-note structure below.  The four note types are:
 warnings - generated warnings
 failures - criteria which are not met, but not a Lisp error
 errors - Lisp errors
 info - supplimentary information
Each of these fields is a list; warnings, failures and errors are check-note
instances, and the info field is of any value."
  (group-name *nst-group-name*)
  (check-name *nst-check-name*)
  (warnings nil) (failures nil) (errors nil) (info nil))

(defun calibrate-check-result (r)
  (with-accessors ((passing-count result-stats-passing)
		   (erring-count result-stats-erring)
		   (failing-count result-stats-failing)
		   (warning-count result-stats-warning)
		   
		   (warnings check-result-warnings)
		   (failures check-result-failures)
		   (errors check-result-errors)) r
    (cond
      (errors (setf erring-count 1))
      (failures (setf failing-count 1))
      (t (setf passing-count 1)))
    (when warnings (setf warning-count 1)))
  r)

(defun check-result (&rest args)
  (calibrate-check-result (apply #'make-check-result args)))

(defparameter *nst-report-driver* nil
  "Control parameter for building report structures.  Should not be reset from
nil at the top level; set via dynamically-scoped bindings.")

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
	   (t (format s "~@<Check ~a ~:[failed~;passed~]: ~
                         ~@{~:[~2*~;~:@_~a~{~:@_ - ~w~}~]~}~:>"
		check-name succeeded
		errors "Errors:" errors  failures "Failures:" failures
		warnings "Warnings:" warnings
		info "Additional information:" info)))))))

(defstruct (context-layer (:type vector) :named)
  "A record of test criterion
 criterion - the criterion symbol itself
 criterion-args - arguments to the criterion
 given-stack - the stack of values assessed by the criterion"
  criterion criterion-args given-stack)

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

(set-pprint-dispatch 'check-note
  #'(lambda (s cn) 
      (with-accessors ((context check-note-context)
		       (stack check-note-stack)
		       (format check-note-format)
		       (args check-note-args)) cn
	(declare (ignorable context stack))
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

(defmacro count-nonnulls (&rest bools)
  (let ((b (gensym)))
    `(loop for ,b in ,bools sum (if ,b 1 0))))

;;;
;;; Build reports after test runs.
;;;

(defun package-report (&optional (package *package*))
  "Top-level function for reporting the results of a package."
  (let* ((result (make-package-result))
	 (user-package (find-package package))
	 (sym-pack (loop for k being the hash-keys
			 of (gethash user-package +package-groups+)
			 collect k)))
    (case *nst-verbosity*
      ((:vverbose)
       (format t "Reporting for actual package ~s~%" user-package)
       (format t "sym-pack ~s~%" sym-pack)))
    (when sym-pack
      (with-accessors ((name package-result-package-name)
		       (checks package-result-group-results)) result
	(setf name (package-name user-package))
	(loop for remote-group in sym-pack do
	  (let* ((local-group (intern (symbol-name remote-group) user-package))
		 (report (group-report local-group)))
	    (setf (gethash local-group checks) report)
	    (incf (result-stats-elapsed-time result)
		  (result-stats-elapsed-time report))
	    (incf (result-stats-tests result)   (result-stats-tests report))
	    (incf (result-stats-passing result) (result-stats-passing report))
	    (incf (result-stats-erring result)  (result-stats-erring report))
	    (incf (result-stats-failing result) (result-stats-failing report))
	    (incf (result-stats-warning result)
		  (result-stats-warning report))))))
    result))

(defun group-report (group)
  "Top-level function for reporting the results of a group."
  (let ((result (make-group-result)))
    (with-accessors ((name group-result-group-name)
		     (checks group-result-check-results)) result
      (setf name group)
      (loop for test in (test-names group)
	    for report = (test-report group test)
	    do
	 (setf (gethash test checks) report)
	 (cond
	   (report
	    (incf (result-stats-elapsed-time result)
		  (result-stats-elapsed-time report))
	    (incf (result-stats-tests result)   (result-stats-tests report))
	    (incf (result-stats-passing result) (result-stats-passing report))
	    (incf (result-stats-erring result)  (result-stats-erring report))
	    (incf (result-stats-failing result) (result-stats-failing report))
	    (incf (result-stats-warning result) (result-stats-warning report)))
	   (t (incf (result-stats-tests result))))))
    result))

(defun test-report (group test)
  "Top-level function for reporting the results of a test."
  (gethash (canonical-storage-name (standalone-class-name group test))
	   +results-record+))

(defun multiple-report (packages groups tests &key system)
  (let* ((package-reports (loop for p in packages collect (package-report p)))
	 (group-reports (loop for g in groups collect (group-report g)))
	 (test-reports (loop for (g . ts) in tests collect (test-report g ts)))
	 (result (make-multi-results :package-reports package-reports
				     :group-reports group-reports
				     :test-reports test-reports
				     :system system)))
    (finish-multiple-report result)))

(defun finish-multiple-report (result)
  (with-accessors ((package-reports multi-results-package-reports)
		   (group-reports multi-results-group-reports)
		   (test-reports multi-results-test-reports)) result
    (loop for report-set in (list package-reports group-reports test-reports)
	  do
       (loop for report in report-set do
	 (incf (result-stats-elapsed-time result)
	       (result-stats-elapsed-time report))
	 (incf (result-stats-tests result)   (result-stats-tests report))
	 (incf (result-stats-passing result) (result-stats-passing report))
	 (incf (result-stats-erring result)  (result-stats-erring report))
	 (incf (result-stats-failing result) (result-stats-failing report))
	 (incf (result-stats-warning result) (result-stats-warning report))))
    result))

(defun all-package-report ()
  (let ((package-hash (make-hash-table :test 'eq)))
    (loop for package-name being the hash-values
	  of +storage-name-to-test-package+
	  do
       (setf (gethash package-name package-hash) t))
    (multiple-report (loop for package-name being the hash-keys of package-hash
			   collect (find-package package-name))
		     nil nil)))

(defun all-groups-report ()
  (let ((group-hash (make-hash-table :test 'eq)))
    (loop for test-report being the hash-values of +results-record+ do
      (when test-report
	(setf (gethash (check-result-group-name test-report) group-hash) t)))
    (multiple-report nil 
		     (loop for group-name being the hash-keys of group-hash
			 collect group-name)
		     nil)))

(defun all-tests-report ()
  (let ((test-reports (loop for test-report being the hash-values
			    of +results-record+
			    if test-report collect test-report)))
    (finish-multiple-report (make-multi-results :package-reports nil
						:group-reports nil
						:test-reports test-reports
						:system nil))))

;;;
;;; Printing functions
;;;

(defun report-package (&optional
		       (package *package*)
		       (stream *nst-output-stream*)
		       (*nst-local-verbosity* *nst-report-default-verbosity*))
  "Top-level function for reporting the results of the tests in a package."
  (let ((*nst-report-driver* :package))
    (format stream "~w" (package-report package))))

(defun report-group (group
		     &optional
		     (stream *nst-output-stream*)
		     (*nst-local-verbosity* *nst-report-default-verbosity*))
  "Top-level function for reporting the results of the tests in a group."
  (let ((*nst-report-driver* :group))
    (format stream "~w" (group-report group))))

(defun report-test (group
		    test &optional
		    (stream *nst-output-stream*)
		    (*nst-local-verbosity* *nst-report-default-verbosity*))
  "Top-level function for reporting the results of a test."
  (let ((*nst-report-driver* :test))
    (format stream "~w" (test-report group test))))

(defun report-multiple (packages groups tests &key
				 (stream *nst-output-stream*)
				 (verbosity *nst-report-default-verbosity*)
				 (system nil system-supp-p))
  "Top-level function for reporting the results of several tests."
  (let ((report (apply #'multiple-report
		       packages groups tests
		       (cond
			 (system-supp-p `(:system ,system))
			 (t nil))))
	(*nst-local-verbosity* verbosity))
    (declare (special *nst-local-verbosity*))
    (format stream "~w" report)))

(defun nst-dump (&key (stream *nst-output-stream*)
		      (verbosity *nst-report-default-verbosity*))
  "Spit out the full NST state."
  (let ((report (all-package-report))
	(*print-pretty* t) (*print-readably* nil)
	(*nst-local-verbosity* verbosity))
    (declare (special *nst-local-verbosity*))
    (format stream "NST globals:~%")
    (format stream " - *nst-verbosity*: ~s~%" *nst-verbosity*)
    (format stream " - *nst-local-verbosity*: ~s~%" *nst-local-verbosity*)
    (format stream " - *nst-report-default-verbosity*: ~s~%" *nst-report-default-verbosity*)
    (format stream " - *nst-output-stream*: ~s~%" *nst-output-stream*)
    (format stream " - *debug-on-error*: ~s~%" *debug-on-error*)
    (format stream " - *nst-info-shows-expected*: ~s~%" *nst-info-shows-expected*)
    (format stream "Stored test results:~%")
    (format stream "~w" report)))

#|
;;; This function has been broken up into the report-multiple above,
;;; and the multi-report struct and function.  Keeping around for just
;;; alittle while for looting code snippets.

(defun report-multiple (packages groups tests &key
				 (stream *nst-output-stream*)
				 (verbosity *nst-report-default-verbosity*)
				 (system nil system-supp-p))
  (let ((*nst-local-verbosity* verbosity))
    (declare (special *nst-local-verbosity*))
    (when system-supp-p
      (format stream "~%Summary of results for system ~a:~%"
	(slot-value system 'asdf::name)))
    (let ((reports
	   (nconc (loop for p in packages
		      for report = (package-report p)
		      collect (let ((*nst-report-driver* :package))
				(format stream "~w~%" report)
				report))
		  (loop for g in groups
		      for report = (group-report g)
		      collect (let ((*nst-report-driver* :group))
				(format stream "~w~%" report)
				report))
		  (loop for (g . ts) in tests
		      for report = (test-report g ts)
		      collect (let ((*nst-report-driver* :test))
				(format stream "~w~%" report)
				report)))))
      (multiple-value-bind (code total passed erred failed warned)
	  (result-summary reports)
	(declare (ignorable code))
	(format stream
	    "TOTAL: ~d of ~d passed (~d failed, ~d error~p, ~d warning~p)~%"
	  passed total failed erred erred warned warned)))))
|#
