;;; File globals.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
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

;;; This file contains settings, global variables and flags.

;;; ----------------------------------------------------------------------

;;;
;;;  Singleton classes.
;;;
;;; Adapted from Tim Bradshaw's example singleton-class.lisp.


(defclass singleton-class (standard-class)
  ((singleton :accessor singleton :initform nil)))

(defmethod validate-superclass ((class singleton-class)
                                (superclass standard-class))
  ;; it's OK for a standard class to be a superclass of a singleton
  ;; class
  t)

(defmethod validate-superclass ((class singleton-class)
                                (superclass singleton-class))
  ;; it's OK for a singleton class to be a subclass of a singleton class
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass singleton-class))
  ;; but it is not OK for a standard class which is not a singleton class
  ;; to be a subclass of a singleton class
  nil)

(defmethod make-instance ((class singleton-class) &key)
  (with-accessors ((singleton singleton)) class
    (unless singleton
      (setf singleton (call-next-method)))
    singleton))

;;;
;;;  Flags and dynamic variable declarations.
;;;
(defvar *nst-verbosity* 1
  "User variable determining how verbose NST's output to the REPL should be.  Internally, this variable takes an integer value: 0 and below are silent, 1 is the default, 2 and 3 are more verbose.  The command-line interpreter assigns keywords to these values, from most terse to most verbose: :silent, nil, :quiet, :default, t, :verbose, :vverbose (:quiet and :default are the same, and are the initial setting).")

(defvar *default-report-verbosity* 2
  "User variable determining the default value for *nst-verbosity* when printing reports (2 by default).")

(defvar *debug-on-error* nil
  "User variable: if non-null, will break into the Lisp REPL debugger upon encountering an unexpected error.  If t, will record the error and continue with other tests.")

(defvar *nst-debug* nil
  "User variable: apply customizable debugging settings.")

(defvar *default-debug-config*
    '(:nst-set ((:debug-on-error t) (:verbose :vverbose)))
  "User variable: the default setting applied by default.  Should be a list of
alternating keyword/forms matching:
 - nst-set - list of lists, each with arguments to :nst :set
 - progn   - list of forms to be evaluated")

(defvar *default-debug-protect* nil)

(defparameter *nst-info-shows-expected* nil
  "Debugging-oriented user flag: when tracing NST structures, print expected
values as hardcoded by the macros, rather than recalled via the generic
functions whose methods the macros define.")

(defparameter *nst-check-user-name* nil
  "Dynamic variable used to set the name of a test in its result report.")
(defparameter *nst-check-internal-name* nil
  "Dynamic variable used to set the name of a test in its result report.")
(defparameter *nst-group-name* nil
  "Dynamic variable used to set the name of the group in a test result report.")

(defparameter *nst-context-evaluable* nil
  "Dynamic-scoped variable tracking whether the values under test should be asusmed evaluated.  Used in preparing context expressions.")
(defparameter *nst-context* nil
  "Dynamic-scoped variable recording the values under test - a list of
context-layer instances.")
(defparameter *nst-stack* nil
  "Dynamic-scoped variable - the stack of values under test by the
current criterion.")

(defparameter *nst-report-driver* nil
  "Dynamic-scoped variable - one of :multiple, :package, :group or :test to determine the top-level form of a report.  Used as a control parameter for printing reports.")

(defvar *nst-output-stream* *standard-output*
  "User variable determining the output stream to which NST should print its output (*standard-output* by default).")

(defparameter *nst-group-shown* nil
  "Dynamic-scoped variable tracking whether the name of a group has been printed, so that tests need not repeat it.")

(defmacro protect-nst-config (&body forms)
  `(let ((*nst-verbosity* *nst-verbosity*)
         (*default-report-verbosity* *default-report-verbosity*)
         (*debug-on-error* *debug-on-error*)
         (*nst-info-shows-expected* *nst-info-shows-expected*)
         (*nst-output-stream* *nst-output-stream*))
     (declare (special *nst-verbosity*  *default-report-verbosity*
                       *debug-on-error* *nst-info-shows-expected*
                       *nst-output-stream*))
     ,@forms))

(defmacro apply-debug-options (forms-spec protect-vars &body forms)
  (let ((protects (gensym)))
    `(protect-nst-config
      (let ((,protects (make-hash-table :test 'eq)))
        (cond
          (*nst-debug*
           (destructuring-bind (&key nst-set progn) ,forms-spec
             (declare (ignorable nst-set progn))
             (loop for (name val) in nst-set do (run-nst-command :set name val))
             (loop for form in progn do
               (eval form))
             (loop for (var-name . package-name) in ,protect-vars do
               (when (boundp var-name)
                 (setf (gethash (intern (symbol-name var-name)
                                        (find-package package-name))
                                ,protects)
                       (symbol-value var-name))))))
          (t nil))
        (prog1 (progn ,@forms)
          (when *nst-debug*
            (loop for (var-name . package-name) in ,protect-vars do
              (when (boundp var-name)
                (setf (symbol-value var-name)
                      (gethash (intern (symbol-name var-name)
                                       (find-package package-name))
                               ,protects))))))))))

(defmacro apply-default-debug-options (&body forms)
  `(apply-debug-options *default-debug-config* *default-debug-protect*
      ,@forms))

;;
;; Management of global properties.
;;
(defgeneric set-nst-property (name value)
  (:method (name value)
     (declare (ignorable value))
     (format t "No such property ~s~%" name))
  (:method ((name (eql :debug-on-error)) value)
     (setf *debug-on-error* value)))
(defgeneric nst-repl-property-doc (n)
  (:documentation "Return the documentation string of an NST property."))
(defgeneric nst-repl-property-display (n)
  (:documentation
   "Return the display value of an NST property's internal value."))
(defgeneric nst-repl-property-encode (prop val)
  (:documentation
   "Encode an NST property's display value as an internal value."))
