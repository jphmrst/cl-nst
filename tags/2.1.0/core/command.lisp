;;; File command.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
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

;;; This file defines the interactive REPL commands.

;;; ----------------------------------------------------------------------

;;; Function version of the command-line interpreter.  The main logic
;;; is here; further below we define platform-specific command-line
;;; interfaces.

(defpackage :nst-artifact-lookup-package
    (:documentation "Auxiliary package for canonical NST command names."))

(defvar *last-repl-call* '(:help))
(defgeneric consider-repl-call-save (name args)
  (:method (name args) (declare (ignorable name args))))

(defgeneric run-command-actual (command &rest args)
  (:documentation "Top-level command interpreter for the NST tester")
  (:method (command &rest args)
     (declare (ignorable args))
     (cond
       ((and (symbolp command)
             (not (eq (find-package :nst-artifact-lookup-package)
                      (symbol-package command))))
        (apply #'run-command-actual
               (intern (symbol-name command) :nst-artifact-lookup-package)
               args))
       (t
        (format t "Unrecognized NST command ~s~%~
                   Use :nst :help for a list of NST commands." command))))
  (:method :before (command &rest args) (consider-repl-call-save command args)))

(defgeneric nst-short-help (command)
  (:documentation "Return the short help message for an NST REPL command.")
  (:method (command)
     (cond
       ((and (symbolp command)
             (not (eq (find-package :nst-artifact-lookup-package)
                      (symbol-package command))))
        (nst-short-help (intern (symbol-name command)
                                :nst-artifact-lookup-package)))
       (t
        (format t "Unrecognized NST command ~s~%~
                   Use :nst :help for a list of NST commands." command)))))

(defgeneric nst-long-help (command)
  (:documentation "Return the long help message for an NST REPL command.")
  (:method (command)
     (cond
       ((and (symbolp command)
             (not (eq (find-package :nst-artifact-lookup-package)
                      (symbol-package command))))
        (nst-long-help (intern (symbol-name command)
                               :nst-artifact-lookup-package)))
       (t
        (format t "Unrecognized NST command ~s~%~
                   Use :nst :help for a list of NST commands." command)))))

(defgeneric nst-arg-names (command)
  (:documentation "Return a string giving help for the argument names for an ~
                   NST REPL command.")
  (:method (command)
     (cond
       ((and (symbolp command)
             (not (eq (find-package :nst-artifact-lookup-package)
                      (symbol-package command))))
        (nst-arg-names (intern (symbol-name command)
                               :nst-artifact-lookup-package)))
       (t
        (format t "Unrecognized NST command ~s~%~
                   Use :nst :help for a list of NST commands." command)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'prep-arg-names-help)
    (defgeneric prep-arg-names-help (arg-list)
      (:documentation "This function formats the lambda list of an
NST REPL command for display in the online-help system.  The macro
def-nst-interactive-command that expands a REPL command definition
into the underlying Lisp defmethods uses this function to generate
the bodies of the help-related methods, so this function must be
available from compile-time forward.")

      (:method (arg-list &aux (in-macrolist t) in-keylist)
         (labels ((prep-arg-name (arg)
                    (cond
                     ((eq arg '&optional)
                      (setf in-macrolist nil in-keylist t)
                      arg)
                     ((eq arg '&key)
                      (setf in-macrolist nil in-keylist t)
                      arg)
                     ((eq arg '&rest)
                      (setf in-macrolist nil in-keylist nil)
                      arg)
                     ((symbolp arg)
                      (string-upcase (symbol-name arg)))
                     ((stringp arg)
                      (string-upcase arg))
                     ((listp arg)
                      (cond
                       (in-keylist (prep-arg-name (car arg)))
                       (in-macrolist
                        (format nil "(~a)" (prep-arg-names-help arg)))
                       (t (format nil "~a" arg))))
                     (t (format nil "~a" arg)))))
           (with-output-to-string (out)
             (format out "~{~a~^ ~}"
               (loop for arg in arg-list
                   collect (prep-arg-name arg)))))))))

(defvar +nst-repl-commands+ nil)
(defvar +nst-repl-properties+ nil)
(defmacro def-nst-interactive-command ((name &key short-help
                                             (long-help nil long-help-supp-p)
                                             (long-help-special
                                              nil long-help-special-supp-p)
                                             (args nil args-supp-p)
                                             (repeatable))
                                       &body forms)
  (let* ((canonical (intern (symbol-name name) :nst-artifact-lookup-package))
         (args-var (gensym))
         (command-run-forms (if args-supp-p
                                `((destructuring-bind ,args ,args-var ,@forms))
                                forms)))
    `(progn
       (defmethod run-command-actual ((cmd (eql ',canonical)) &rest ,args-var)
         ,@(unless args-supp-p `((declare (ignorable ,args-var))))
         (block nst-command
           (handler-bind ((nst-error #'(lambda (e)
                                         (format t "~w~%" e)
                                         (return-from nst-command))))
             ,@command-run-forms)))
       ,(when repeatable
          `(defmethod consider-repl-call-save ((cmd (eql ,name)) args)
             (setf *last-repl-call* (cons cmd args))))
       (defmethod nst-arg-names ((cmd (eql ',canonical)))
         ,(prep-arg-names-help args))
       (defmethod nst-short-help ((cmd (eql ',canonical)))
         ,short-help)
       (defmethod nst-long-help ((cmd (eql ',canonical)))
         ,@(cond
            (long-help-special-supp-p long-help-special)
            (long-help-supp-p (list long-help))
            (t (list short-help))))
       (unless (member ,name +nst-repl-commands+)
         (setf +nst-repl-commands+ (nconc +nst-repl-commands+ (list ,name)))))))

(defmacro def-nst-property (name variable &key (doc "")
                                 (filter '(lambda (x) x))
                                 (unfilter '(lambda (x) x)))
  `(progn
     (defmethod set-nst-property ((name (eql ,name)) value)
       (setf ,variable (nst-repl-property-encode ,name value))
       (format t "Set property ~a to ~s~%" ',name value))
     (defmethod show-nst-property ((name (eql ,name)))
       (format t "Property ~a is set to ~s~%"
         ',name (funcall #',unfilter ,variable)))
     (defmethod nst-repl-property-doc ((n (eql ,name)))
       ,doc)
     (defmethod nst-repl-property-encode ((n (eql ,name)) value)
       (funcall #',filter value))
     (defmethod nst-repl-property-display ((n (eql ,name)))
       (funcall #',unfilter ,variable))
     (unless (member ,name +nst-repl-properties+)
       (push ,name +nst-repl-properties+))))

(defun flag-filter (x) (if x t nil))
(def-nst-property :debug-on-error *debug-on-error*
  :doc "When non-nil, break into the debugger when NST encounters an error."
  :filter flag-filter)
(def-nst-property :debug-on-fail *debug-on-fail*
  :doc "When non-nil, break into the debugger when NST encounters an error."
  :filter flag-filter)
(def-nst-property :verbose *nst-verbosity*
  :doc "Valid settings: :silent (aka nil), :quiet (aka :default), :verbose, ~
        (aka t), :vverbose"
  :filter (lambda (x)
            (case x
              ((:silent nil)     0)
              ((:default :quiet) 1)
              ((t :verbose)      2)
              ((:vverbose)       3)
              ((:trace)          4)
              (t (error "Invalid value ~s" x))))
  :unfilter (lambda (x)
              (cond
                ((< x 1)   :silent)
                ((eql x 1) :quiet)
                ((eql x 2) :verbose)
                ((eql x 3) :vverbose)
                ((> x 3)   :trace))))

#+allegro
(def-nst-property :backtraces *generate-backtraces*
  :doc
  "When non-nil, attempts to capture the Lisp backtrace of errors in tests."
  :filter   (lambda (x) (if x t nil))
  :unfilter (lambda (x) (if x t nil)))

(def-nst-interactive-command (:help :short-help "Print a list of commands."
                                    :long-help "Print this help message.")
    (format t "-----------------------------------------------------~%~
               NST unit testing system --- interactive REPL commands~%~
               -----------------------------------------------------~%")
  (loop for cmd in +nst-repl-commands+ do
    (format t "~%~s ~a~%~a~%" cmd (nst-arg-names cmd) (nst-short-help cmd)))
  (format t "~%Use~%  :nst :COMMAND :help~%for more information about a ~
             particular command.~%~%Without an explicit command, :nst repeats ~
             the last interesting command~%(currently, ~s~{ ~s~})"
    :nst *last-repl-call*))

(def-nst-interactive-command
    (:debug :short-help "Activate NST debugging."
            :args (&optional (val t))
            :long-help "Activate NST debugging by activating *default-debug-config* before each test operation.")
  (cond
    ((and *nst-debug* val) (format t "NST debugging is already active.~%"))
    (val          (format t "Activating NST debugging.~%"))
    (*nst-debug*  (format t "Deactivated NST debugging.~%"))
    (t            (format t "NST debugging is already deactivated.~%")))
  (setf *nst-debug* val))

(def-nst-interactive-command
    (:open :short-help "Inject fixtures into the current name space."
           :args (&rest fixtures)
           :repeatable t)
    (let ((*open-via-repl* t))
      (declare (special *open-via-repl*))
      (loop for fixture in fixtures do
        (open-fixture fixture))))

(def-nst-interactive-command
    (:run-package :short-help "Run all NST tests stored in the given packages."
                  :args (&rest packages)
                  :repeatable t)
    (unless packages
      (setf packages (list *package*)))
    (apply-default-debug-options
     (let (ran-packages)
       (loop for package in packages do
         (cond
           ((find-package package)
            (push package ran-packages)
            (run-package package) )
           (t (format t "No such package ~a~%" package))))
       (report-multiple (nreverse ran-packages) nil nil))))

(def-nst-interactive-command
    (:run-group :short-help "Run all NST tests in the given groups."
                :args (&rest groups)
                :repeatable t)
  (apply-default-debug-options
   (loop for group in groups do (run-group group))
   (report-multiple nil groups nil)))

(def-nst-interactive-command
    (:run-test :short-help "Run a single NST test."
               :args (group test)
               :repeatable t)
  (apply-default-debug-options
   (run-test group test)
   (report-multiple nil nil (list (cons group test)))))

(def-nst-interactive-command
    (:run :short-help "Run NST packages, groups and tests."
          :args (&rest stuff)
          :repeatable t)
  (apply-default-debug-options
   (let (report-packages report-groups report-tests)
     (loop for id in stuff do
       (let ((interps (lookup-artifact id)))
         (cond
          ((null interps)
           (format t "There is no NST-testable unit with the name ~a.~%" id))

          ((> (length interps) 1)
           (format t "~a is ambiguous; try again with one of:~%" id)
           (loop for interp in interps do
             (cond
              ((packagep interp)
               (format t " - :nst :run-package :~a~%" id))

              ((group-record-p interp)
               (format t " - :nst :run-group ~s~%" (group-name interp)))

              (t
               (format t " - :nst :run-test ~s ~s~%"
                 (group-name interp) (test-name-lookup interp))))))

          (t (let ((interp (car interps)))
               (cond
                ((packagep interp)
                 (push interp report-packages)
                 (run-package interp))

                ((group-record-p interp)
                 (push interp report-groups)
                 (run-group-inst interp))

                ((test-record-p interp)
                 (push interp report-tests)
                 (run-test-inst interp))

                (t
                 (error
                  "Unrecognizable artifact ~s returned by lookup-artifact"
                  interp))))))))
     (when (or report-packages report-groups report-tests)
       (report-multiple report-packages report-groups report-tests)))))

(def-nst-interactive-command
    (:report :short-help "Show a summary of test results."
             :long-help "Show a summary of test results.  Usage:
  :nst :report PACKAGE
  :nst :report GROUP
  :nst :report GROUP TEST
  :nst :report
The last form summarizes all interesting results."
             :args (&optional (group-or-package nil gp-supp-p)
                              (test nil test-supp-p))
             :repeatable nil)
  (report-summary group-or-package gp-supp-p test test-supp-p))

(def-nst-interactive-command
    (:detail :short-help "Show detailed test results."
             :long-help "Show detailed test results.  Usage:
  :nst :detail PACKAGE
  :nst :detail GROUP
  :nst :detail GROUP TEST
  :nst :detail
The last form shows all interesting results."
             :args (&optional (group-or-package nil gp-supp-p)
                              (test nil test-supp-p))
             :repeatable nil)
  (report-details group-or-package gp-supp-p test test-supp-p))

(def-nst-interactive-command
    (:clear :short-help "Clear test results."
            :args ())
  (clrhash +results-record+)
  (format *standard-output* "Results cleared."))

(def-nst-interactive-command
    (:set :short-help "Set or show an NST property."
          :long-help-special
          ((with-output-to-string (*standard-output*)
             (format t "Set or show an NST property.  Available properties:~%")
             (loop for prop in +nst-repl-properties+ do
               (format t "~%~s~%~a~%" prop (nst-repl-property-doc prop)))))
          :args (name &optional (value nil value-supp-p)))
    (cond
      (value-supp-p (set-nst-property name value))
      (t (show-nst-property name))))

(def-nst-interactive-command
    (:undef :short-help "Un-define an NST group or test"
            :args (group &optional (test nil test-supp-p)))
    (cond
      (test-supp-p
       (let* ((group-obj (make-instance group))
              (test-obj (gethash test (test-name-lookup group-obj))))
         (setf (test-list group-obj)
               (delete (check-group-name test-obj) (test-list group-obj)))
         (remhash test (test-name-lookup group-obj))))

      (t (let ((package-hash (gethash (symbol-package group) +package-groups+)))
           (cond
            ((and package-hash (gethash group package-hash))
             (remhash group package-hash)
             (let ((this-name-use (get-name-use-record group)))
               (setf (name-use-group this-name-use) nil))
             ;; Undo (note-executable ',group-name ,*group-object-variable*)
             ;; TO DO
             )
            (t (format t "No such group ~s.~%" group)))))))

(def-nst-interactive-command
    (:unset :short-help "Clear an NST property." :args (name))
    (set-nst-property name nil))

(def-nst-interactive-command
    (:whatis :short-help "Query what NST artifacts a name denotes" :args (name))
    (let ((usage (get-name-use name)))
      (cond
        ((null usage) (format t "The symbol ~s is not known to NST" name))
        ((null (cdr usage)) (format t "~a~%" (car usage)))
        (t (format t "There are ~d uses of ~s:~%~{ - ~a~%~}"
             (length usage) name usage)))
      nil))

(def-nst-interactive-command
    (:apply :short-help "Apply a criterion to forms"
            :args (criterion &rest forms))
    (format t "~w~%" (check-subcriterion-on-form criterion `(list ,@forms))))

(defun run-nst-command (&rest args)
  (cond
    ((null args)
     (apply #'run-command-actual *last-repl-call*))

    (t
     (destructuring-bind (command-name &rest command-args) args
       (cond
         ((eq :help (car command-args))
          (format t "NST command ~s~:*~%Format:~%  :nst ~s ~a~%~%~a"
            command-name (nst-arg-names command-name)
            (nst-long-help command-name)))

         (t (apply #'run-command-actual command-name command-args)))))))

(defmacro nst-cmd (&rest args)
  "Run an NST command."
  `(apply #'run-nst-command ',args))

;;; Platform-specific command-line interpreter interfaces.

#+(or allegro sbcl)
(#+allegro top-level:alias #+sbcl sb-aclrepl:alias "nst" (&rest args)
           (apply #'run-nst-command args))
