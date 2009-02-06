;;; File command.lisp
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

;;; This file contains the interactive command language.

;;; ----------------------------------------------------------------------

(defvar *nst-level* :package)
(defvar *nst-unit* nil)
(defvar *nst-test* nil)

;;;(defun run-nst-commands (&rest args)
;;;  "Top-level command interpreter for the NST tester"
;;;  (block runner
;;;    
;;;    (unless args 
;;;      (case *nst-level*
;;;	(:package
;;;	 (run-package (if *nst-unit* *nst-unit* *package))
;;;	 (report-package (if *nst-unit* *nst-unit* *package)))
;;;	
;;;	(:group
;;;	 (run-group *nst-unit*)
;;;	 (report-group *nst-unit*))
;;;	
;;;	(:test
;;;	 (run-test *nst-unit* *nst-test*)
;;;	 (report-test *nst-unit* *nst-test*))
;;;	
;;;	(otherwise
;;;	 (format t "No tests scheduled.~%")))
;;;      
;;;      (return-from runner))
;;;
;;;    (loop do
;;;      (if (null args) (return-from runner))
;;;
;;;      (block single-command
;;;	(let ((head (pop args)))
;;;	  (macrolet
;;;	      ((pop-arg (want have)
;;;		 `(cond 
;;;		    ((null args)
;;;		     (format t "Command ~s requires ~d~:* ~
;;;                               argument~[s~;~:;s~] but given ~d.~%"
;;;			     head ,want ,have)
;;;		     (return))
;;;		    (t (pop args))))
;;;
;;;	       (command-case-core (synonyms args &rest forms)
;;;		 (let* ((want (length args))
;;;			(arg-bindings
;;;			 (loop for arg in args and have from 0
;;;			       collect
;;;			       (list arg (list 'pop-arg want have)))))
;;;		   `(when (member head ',synonyms)
;;;		      (let ,arg-bindings ,@forms))))
;;;	       
;;;	       (command-case (synonyms args &rest forms)
;;;		   `(command-case-core ,synonyms ,args ,@forms
;;;				       (return-from single-command)))
;;;
;;;	       (command-case-flag-setter (synonyms variable blurb)
;;;		 (let ((flag (gensym)))
;;;		   `(command-case ,synonyms (,flag)
;;;			(setf ,variable ,flag)
;;;			(format t "~:[Deactivated~;Activated~] ~a.~%"
;;;				,flag ,blurb))))
;;;	       
;;;	       (warn-unimplemented (&rest forms)
;;;		 `(progn ,@forms
;;;			 (format t "Command ~s not implemented~%"
;;;				 head))))
;;;
;;;	    (command-case-core (:help help h) ()
;;;		(format t "~a" (nst-top-help))
;;;		(return-from runner))
;;;	    
;;;	    (command-case-flag-setter (:verbose) *verbose-output*
;;;				      "verbose output")
;;;	    
;;;	    (command-case-flag-setter (:debug) *debug-output*
;;;				      "debugging output")
;;;
;;;	    (command-case-flag-setter (:break-on-wrong)
;;;				      *break-on-wrong*
;;;				      "breaking on test failure")
;;;
;;;	    (command-case-flag-setter (:break-on-error)
;;;				      *break-on-error*
;;;				      "breaking on raised errors")
;;;
;;;	    (command-case-flag-setter (:debug-on-error)
;;;				      *debug-on-error*
;;;				      "debugging on raised errors")
;;;
;;;	    (command-case-flag-setter (:summarize-scheduled)
;;;				      *scheduled-summary-output*
;;;				      "summaries for scheduled runs")
;;;
;;;	    (command-case-flag-setter (:summarize-single)
;;;				      *scheduled-single-output*
;;;				      "summaries for single runs")
;;;
;;;	    (command-case-flag-setter (:open-used)
;;;				      *open-used-fixtures*
;;;				      "opening used fixtures")
;;;
;;;	    (command-case-flag-setter (:reopen)
;;;				      *reopen-fixtures*
;;;				      "reopening fixtures")
;;;
;;;	    (command-case-flag-setter
;;;	     (:defer-test-compile) *defer-test-compile*
;;;	     "deferral of test form compilation by default")
;;;
;;;	    (command-case (:dump dump) () (nst-dump t))
;;;
;;;	    (command-case (:blurb) (group-name test-name)
;;;			  (give-blurb group-name test-name))
;;;
;;;	    (command-case (:blurb-group) (group-name)
;;;              (let ((group-info (gethash group-name +groups+)))
;;;		(loop for test across (get-test-names group-info)
;;;		      do (give-blurb group-name test))))
;;;
;;;	    (command-case (:cancel) ()
;;;	      (setf *interesting-packages* nil
;;;		    *pending-packages* nil
;;;		    *interesting-group-names* nil
;;;		    *pending-group-names* nil)
;;;	      (clrhash *interesting-test-names*)
;;;	      (clrhash *pending-test-names*)
;;;	      (clrhash *erred-groups*)
;;;	      (clrhash *erred-cleanup*)
;;;	      (clrhash *failed-tests*)
;;;	      (clrhash *erred-tests*)
;;;	      (format t "Cancelled all scheduled tests~%"))
;;;
;;;	    (command-case (:p) (package-name)
;;;		(let ((package (find-package package-name)))
;;;		  (if package
;;;		      (progn
;;;			(unless (member package
;;;					*interesting-packages*)
;;;			  (push package *interesting-packages*))
;;;			(unless (member package *pending-packages*)
;;;			  (push package *pending-packages*))
;;;			(format t "Marked package ~s for testing~%"
;;;				package-name))
;;;		      (format t "ERROR: cannot find package ~s~%"
;;;			      package-name))))
;;;
;;;	    (command-case (:np) (package-name)
;;;	      (let ((package (find-package package-name)))
;;;		(if package
;;;		  (progn
;;;		    (when (member package *interesting-packages*)
;;;		      (setf *interesting-packages*
;;;			    (delete package *interesting-packages*)))
;;;		    (when (member package *pending-packages*)
;;;		      (setf *pending-packages*
;;;			    (delete package *pending-packages*)))
;;;		    (format t "Removed package ~s from testing~%"
;;;		      package-name))
;;;		  (format t "ERROR: cannot find package ~s~%"
;;;		    package-name))))
;;;
;;;	    (command-case (:g) (group-name)
;;;		(if (gethash group-name +groups+)
;;;		    (progn
;;;		      (unless (member group-name
;;;				      *interesting-group-names*)
;;;			(push group-name
;;;			      *interesting-group-names*))
;;;		      (unless (member group-name
;;;				      *pending-group-names*)
;;;			(push group-name *pending-group-names*))
;;;		      (format t "Marked group ~s for testing~%"
;;;			      group-name))
;;;		    (format t "ERROR: cannot find group ~s~%"
;;;			    group-name)))
;;; 
;;;	    (command-case (:ng) (group-name)
;;;		(if (gethash group-name +groups+)
;;;		    (progn
;;;		      (when (member group-name
;;;				      *interesting-group-names*)
;;;			(setf *interesting-group-names*
;;;			      (delete group-name
;;;				*interesting-group-names*)))
;;;		      (when (member group-name
;;;				    *pending-group-names*)
;;;			(setf *pending-group-names*
;;;			      (delete group-name *pending-group-names*)))
;;;		      (format t "Removed group ~s from testing~%"
;;;			group-name))
;;;		    (format t "ERROR: cannot find group ~s~%"
;;;		      group-name)))
;;; 
;;;	    (command-case (:t) (group-name test-name)
;;;		(if (gethash group-name +groups+)
;;;		    (progn
;;;		      (let ((i-tests
;;;			     (gethash group-name 
;;;				      *interesting-test-names*))
;;;			    (p-tests
;;;			     (gethash group-name *pending-test-names*)))
;;;			(unless i-tests
;;;			  (setf i-tests (make-hash-table)
;;;				(gethash group-name
;;;					 *interesting-test-names*)
;;;				i-tests))
;;;			(unless p-tests
;;;			  (setf p-tests (make-hash-table)
;;;				(gethash group-name *pending-test-names*)
;;;				p-tests))
;;;			(setf (gethash test-name i-tests) t
;;;			      (gethash test-name p-tests) t))
;;;		      (when *verbose-output*
;;;			(format t "Marked test ~s (group ~s) ~
;;;                                   for testing."
;;;				test-name group-name)))
;;;		    (format t "ERROR: cannot find group ~s~%"
;;;			    group-name)))
;;; 
;;;	    (command-case (:nt) (group-name test-name)
;;;	      (when (gethash group-name +groups+)
;;;		(progn
;;;		  (let ((i-tests (gethash group-name
;;;					  *interesting-test-names*))
;;;			(p-tests (gethash group-name
;;;					  *pending-test-names*)))
;;;		    (when i-tests  (remhash test-name i-tests))
;;;		    (when p-tests  (remhash test-name p-tests))
;;;		    (when (eql (hash-table-count i-tests) 0)
;;;		      (remhash group-name *interesting-test-names*))
;;;		    (when (eql (hash-table-count p-tests) 0)
;;;		      (remhash group-name *pending-test-names*)))
;;;		  (when *verbose-output*
;;;		    (format t "Removed test ~s (group ~s) from testing."
;;;		      test-name group-name)))))
;;;
;;;	    (command-case (:run) ()
;;;		(reset-pending) (run-pending) (report-last-run))
;;;
;;;	    (command-case (:continue) ()
;;;		(run-pending) (report-last-run))
;;;
;;;	    (command-case (:run-package) (pkg-name)
;;;		(under-empty-pendings
;;;		 (push (find-package pkg-name) *pending-packages*)))
;;;
;;;	    (command-case (:run-packages) (pkg-name-list)
;;;	      (under-empty-pendings
;;;	       (setf *pending-packages*
;;;		     (loop for pkg-name in pkg-name-list
;;;			   collect (find-package pkg-name)))))
;;;
;;;	    (command-case (:run-group) (group)
;;;	      (under-empty-pendings
;;;	       (push group *pending-group-names*)))
;;;
;;;	    (command-case (:run-groups) (group-name-list)
;;;	      (under-empty-pendings
;;;	       (loop for group-name in group-name-list do
;;;		 (push group-name *pending-group-names*))))
;;;
;;;	    (command-case (:run-test) (group test)
;;;	      (in-empty-context
;;;	       (let ((singleton (make-hash-table)))
;;;		 (setf 
;;;		  (gethash test singleton) t
;;;		  (gethash group 
;;;			   *pending-test-names*) singleton))))
;;;
;;;	    (command-case (:retry) ()
;;;		(loop for group being the hash-keys of *erred-groups*
;;;		      do (push (get-name group) *pending-group-names*))
;;;		(clrhash *erred-groups*)
;;;		
;;;		(loop for group being the hash-keys of *erred-cleanup*
;;;		      do (push (get-name group) *pending-group-names*))
;;;		(clrhash *erred-cleanup*)
;;;		
;;;		(loop for test-hash
;;;		      in (list *failed-tests* *erred-tests*)
;;;		      do 
;;;		   (loop for group being the hash-keys in test-hash
;;;			 using (hash-value test-set)
;;;			 do
;;;		      (let ((new-test-set
;;;			     (gethash group *pending-test-names*)))
;;;			(unless new-test-set
;;;			  (setf new-test-set (make-hash-table)
;;;				(gethash group *pending-test-names*)
;;;				new-test-set))
;;;			(loop for test being the hash-keys in test-set
;;;			      using (hash-value flag)
;;;			      do
;;;			   (when flag
;;;			     (setf (gethash test new-test-set) t))))
;;;		      (remhash group test-hash)))
;;;		(run-pending) (report-last-run))
;;;
;;;	    (command-case (:open) (fixture-name)
;;;	      (handler-case (open-fixture fixture-name)
;;;		(unknown-fixture (cnd)
;;;		  (format t "Can't find fixture ~s ~
;;;                             ~_(check current package)." (name cnd))
;;;		  (return-from run-nst-commands))))
;;;
;;;	    (command-case (:open*) (fixture-names)
;;;              (dolist (fixture-name fixture-names)
;;;	        (handler-case (open-fixture fixture-name)
;;;		  (unknown-fixture (cnd)
;;;		    (format t "Can't find fixture ~s ~
;;;                             ~_(check current package)." (name cnd))
;;;		    (return-from run-nst-commands)))))
;;;
;;;	    (command-case (:open-group-fixtures) (group-name)
;;;	      (labels ((open-one (fixture-name)
;;;			 (handler-case (open-fixture fixture-name)
;;;			   (unknown-fixture (cnd)
;;;			     (format t "Can't find fixture ~s ~
;;;                             ~_(check current package)." (name cnd))
;;;			     (return-from run-nst-commands))))
;;;		       (get-group-info (group-name)
;;;			 (or (gethash group-name +groups+)
;;;			     (format t "Cannot find test-group ~s ~
;;;                                      \(check current package\)"
;;;				    group-name)
;;;			     (return-from run-nst-commands (values)))))
;;;		;; possibly, because of :open using :uses, it's
;;;		;; unnecessary to loop over all group names.  Not
;;;		;; sure.  Writing a search routine to prune extras
;;;		;; seems painful. [2008/04/10:rpg]
;;;		(loop for fixture in (get-fixtures (get-group-info group-name))
;;;		      do (open-one fixture))))
;;;
;;;	    )
;;;	  
;;;	  (format t "Unrecognized NST command ~s~%~
;;;                     For more options, use :nst :help~%~%"
;;;		  head))))))

;;; Platform-specific command-line interpreter interfaces.

;;;#+(or allegro sbcl)
;;;(#+allegro top-level:alias #+sbcl sb-aclrepl:alias "nst" (&rest args)
;;;	   (apply #'run-nst-commands args))

