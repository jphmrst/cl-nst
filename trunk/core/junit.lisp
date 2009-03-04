;;; File junit.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
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

(defgeneric junit-xml-snippet (item &optional stream padding)
  (:documentation "Print XML items corresponding to a test result.  The padding
argument should be a string of just spaces."))

(defmethod junit-xml-snippet ((item multi-results)
			      &optional (s *standard-output*) (padding ""))
  (with-accessors ((elapsed-time result-stats-elapsed-time)
		   (tests result-stats-tests)
		   (errors result-stats-erring)
		   (failures result-stats-failing)
		   (system multi-results-system)
		   (package-reports multi-results-package-reports)
		   (group-reports multi-results-group-reports)
		   (test-reports multi-results-test-reports)) item
    (format s
	"~a<testsuite errors=\"~d\" failures=\"~d\"~@[ name=~s~] ~
                      tests=\"~d\" time=\"~f\"~@[ hostname=~s~]>~%"
      padding errors failures
      (when system
	(cond
	 ((slot-boundp system
		       'asdf::description) (slot-value system
		       'asdf::description))
	 (t (symbol-to-junit-name (slot-value system 'asdf::name)))))
      tests
      (/ elapsed-time internal-time-units-per-second)
      
      ;; The hostname.  This isn't Lisp-standard, so maybe we can't
      ;; have it.
      #+allegro (let ((outputs (excl.osi:command-output "hostname")))
		  (if (and outputs (stringp (car outputs)))
		      (car outputs)))
      #-allegro nil)
    (let ((new-padding (concatenate 'string "  " padding)))
      (loop for reports in (list package-reports group-reports test-reports) do
	(loop for report in reports do
	  (cond
	   (report
	    (junit-xml-snippet report s new-padding))))))
    (format s "~a</testsuite>~%" padding)))

(defmethod junit-xml-snippet ((item check-result)
			      &optional (s *standard-output*) (padding ""))
  (with-accessors ((check-name check-result-check-name)
		   (warnings check-result-warnings)
		   (failures check-result-failures)
		   (errors check-result-errors)
		   (info check-result-info)
		   (elapsed-time check-result-elapsed-time)) item
    (format s "~a<testcase classname=~s time=\"~f\""
      padding (symbol-to-junit-name check-name)
      (/ elapsed-time internal-time-units-per-second))
    (cond
      (errors
       (format s ">~%~a  <error message=\"ERROR\"/>~%" padding)
       (format s "~a</testcase>~%" padding))
      (failures
       (format s ">~%~a  <failure type=\"FAILURE\"/>~%" padding)
       (format s "~a</testcase>~%" padding))
      (t
       (format s " />~%")))))

(defun nst-xml-dump (stream)
  (nst-junit-dump stream))

(defun nst-junit-dump (stream)
  (let ((report (all-tests-report)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>~%")
    (junit-xml-snippet report stream)))

(defun symbol-to-junit-name (symbol)
  (format nil "lisp.~a.~a" (package-name (symbol-package symbol)) (symbol-name symbol)))

#|
(defun nst-xml-dump (stream)
  (macrolet ((group-test-loop (hash group test content &rest forms)
	       (let ((name-hash (gensym "name-hash-")))
		 `(loop for ,group being the hash-keys
			  of ,hash
			  using (hash-value ,name-hash)
			  do
		       (loop for ,test being the hash-keys
			     of ,name-hash
			     using (hash-value ,content)
			     do ,@forms))))
	     (group-test-names-from-hashes (hash)
	       (let ((group (gensym "group-"))
		     (test (gensym "test-"))
		     (name-hash (gensym "name-hash-")))
		 `(loop for ,group being the hash-keys of ,hash
			using (hash-value ,name-hash)
			append
			(loop for ,test being
			      the hash-keys of ,name-hash
			      collect (list ,group ,test))))))
	    
    ; dump failed, erred, and passed tests
    (flet ((hash-keys (ht)
	    (sort (loop for x being the hash-keys of ht collect x)
		  #'string-lessp)))
      (let ((myGroups 
	     (remove-duplicates
	      (apply #'append 
		     *interesting-group-names* 
		     (mapcar (lambda (p) (hash-keys (gethash (find-package p) +groups-by-package+)))
			     *interesting-packages*))))
	    (myFailed (group-test-names-from-hashes *failed-tests*))
	    (myErred  (group-test-names-from-hashes *erred-tests*))
	    (myPassed (group-test-names-from-hashes *passed-tests*)))

    ; dump the header
	(format stream "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>~%")


	(loop for group in *interesting-group-names* do
  ; (loop for group in myGroups do
	      (format stream 
		      "<testsuite errors=\"~d\" failures=\"~d\" name=~s tests=\"~d\">~%"
		      (length myErred)
		      (length myFailed)
		      (symbol-to-junit-name group)
		      (+ (length myErred) (length myFailed) (length myPassed)))
	      
	      
	      (loop for test in myFailed do
		    (format stream 
			    "  <testcase classname=\"~a\" name=\"~s\">~%~
                     <failure type=\"FAILURE\"/>~%~
                  </testcase>~%"
			    (symbol-to-junit-name (first test))
			    (second test)))
	      
	      (loop for test in myErred do
		    (format stream 
			    "  <testcase classname=\"~a\" name=\"~s\">~%~
                     <error message=\"ERROR\"/>~%~
                  </testcase>~%"
			    (symbol-to-junit-name (first test))
			    (second test)))
	      
	      (loop for test in myPassed do
		    (format stream 
			    "  <testcase classname=\"~a\" name=\"~s\"/>~%"
			    (symbol-to-junit-name (first test))
			    (second test)))

    ; dump footer
	      (format stream "   <system-out><![CDATA[~%")
	      (nst-dump stream)
	      (format stream 
		      "]]></system-out>~%~
                <system-err><![CDATA[]]></system-err>~%~
                </testsuite>~%")
	      ))
      )))
|#
