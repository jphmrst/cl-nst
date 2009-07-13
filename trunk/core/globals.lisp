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
;;;  Base classes.
;;;
(defclass group-base-class () ()
  (:documentation "Base class of group behavior."))

(defclass standalone-test-base-class () ()
  (:documentation "Base class of standalone test execution behavior."))

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

(defparameter *nst-check-name* nil
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
;;;
;;; Internal tables.
;;;
(defvar +package-groups+ (make-hash-table :test 'eq)
  "Map from packages to the test groups declared in each package.")

;; Properties of groups.  Many of these function have methods on
;; symbols (presumably class names) that either relay to class
;; methods, or re-dispatch after instantiating an object of the named
;; class.

(defgeneric group-name (group-instance)
  (:documentation "Map from a group instance back to its symbolic name."))

(defgeneric test-names (fixture-or-group)
  (:documentation "The names of tests in a group.  Will be given an eql-method
by the macros which expand tests and groups."))
(add-class-name-instantiator-method test-names)

(defgeneric group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates the class of
group-specific activities.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-class-name)

(defgeneric group-fixture-classes (group-name)
  (:documentation
   "Map from groups to the private names of the group's fixtures."))
(add-class-name-static-method group-fixture-classes)

(defgeneric test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for testing the whole group of
tests.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-in-group-class-name)

(defgeneric standalone-test-in-group-class-name (group-name)
  (:documentation
   "Map from groups to the private name with which NST associates a class with
which every test in the group is associated for a standalone test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method standalone-test-in-group-class-name)

(defgeneric test-fixture-classes (name))
(add-class-name-static-method test-fixture-classes)

(defgeneric package-groups (package-or-symbol))
(defmethod package-groups ((s symbol))
  (package-groups (find-package s)))
(defmethod package-groups ((p package))
  (let ((group-hash (gethash p +package-groups+)))
    (when group-hash
      (loop for g being the hash-keys of group-hash collect g))))

;; Information by Lisp package.

(defgeneric check-name (check-instance)
  (:documentation "Map from a check instance back to its symbolic name."))

(defgeneric suite-test-classes (group-prototype))
(defgeneric standalone-test-classes (group-prototype))
(defgeneric config-test-classes (group-prototype))

(defun suite-class-name (group-name test-name)
  (gethash test-name
           (suite-test-classes (class-prototype (find-class group-name)))))

(defun standalone-class-name (group-name test-name)
  (gethash test-name
           (standalone-test-classes (class-prototype
                                     (find-class group-name)))))

(defun test-config-class-name (group-name test-name)
  (gethash test-name
           (config-test-classes (class-prototype
                                      (find-class group-name)))))

(defgeneric canonical-storage-name (test-name)
  (:documentation
   "Map from various test names and instances to the private name against which
NST associates test results."))

;; Fixture properties and operations.

(defgeneric bound-names (fixture-or-group)
  (:documentation "The names defined by each fixture.  Will be given
an eql-method by the macros which expand tests and groups."))
(add-class-name-static-method bound-names)

(defgeneric group-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a group.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method group-fixture-class-name)

(defgeneric test-fixture-class-name (fixture-name)
  (:documentation
   "Map from fixture names to the private name with which NST associates the
corresponding internal name-binding NST class for adding fixtures to a test.")
  (:method (default) (declare (ignorable default)) nil))
(add-class-name-static-method test-fixture-class-name)

(defgeneric open-fixture (fixture-name &optional package)
  (:documentation
   "Inject the names defined by the named fixture into the given package, by
default the current package."))
(defmethod open-fixture ((s symbol) &optional (in-package *package*))
  (open-fixture (make-instance s) in-package))

(defgeneric anon-fixture-forms (forms))
(add-class-name-static-method anon-fixture-forms)

;; Diagnostic information display.

(defgeneric blurb-context-line (stream id args forms)
  (:documentation "Give a short description of a context."))

(defgeneric detail-context-line (stream id args forms)
  (:documentation "Give a longer blurb of a context."))

(defgeneric stack-transformer (id)
  (:documentation "Check form-specific stack transformation."))

;; Common superclass of all results holders.  We use the accessors
;; below, so keep this definition here rather than in the status file.

(defstruct result-stats "Statistics common to the different result summaries."
  (tests 0) (passing 0) (erring 0) (failing 0) (warning 0)
  (elapsed-time 0)
  (timestamp (multiple-value-list (get-decoded-time))))

;;;
;;; Recording of results.  We use a hash table here --- unlike the
;;; method-based recording of test symbols, we're not worried about
;;; straddling the compile/load/run-time borders for result recording.
;;;
(defvar +results-record+ (make-hash-table :test 'eq)
  "Results of test runs.")

;; Extracting information for debugging.

(defgeneric trace-fixture (fx)
  (:documentation "Provide debugging information about a fixture.")
  (:method (fx) (format t "No known fixture ~s~%" fx)))
(add-class-name-instantiator-method trace-fixture)

(defgeneric trace-group (gr)
  (:documentation "Provide debugging information about a group.")
  (:method (gr) (format t "No known group ~s~%" gr)))
(add-class-name-instantiator-method trace-group)

(defgeneric trace-test (gr ts)
  (:documentation "Provide debugging information about a test.")
  (:method (gr ts) (format t "No known test ~s in group ~s~%" ts gr)))

(defun trace-results ()
  "Internal debugging function: dump the results hash."
  (loop for ts being the hash-keys of +results-record+ using (hash-value rs) do
    (format t "~s -> ~s~%" ts rs)))

;;;
;;; More generic functions whose methods are defined by the various
;;; macros.
;;;

;; Internal test execution functions.

(defgeneric core-run (group-or-test)
  (:documentation
   "Group fixtures provide name-binding :around methods to this generic
function; group setup and cleanup become :before and :after methods.")
  (:method ((group-inst group-base-class))
     (let ((group-name (group-name group-inst)))
       (when (> *nst-verbosity* 3)
         (format t "    Starting run loop for ~s~%" group-inst))
       (loop for test in (test-names group-inst) do
         (when (> *nst-verbosity* 3)
           (format t "      Starting loop entry ~s~%" test))
         (let ((in-suite-class-name (suite-class-name group-name test)))
           (when (> *nst-verbosity* 3)
             (format t "    Suite class name ~s~%" in-suite-class-name)
             (format t "    Actual class ~s~%"
               (find-class in-suite-class-name)))
           (let ((test-inst (make-instance in-suite-class-name)))
             (when (> *nst-verbosity* 3)
               (format t "    Instance ~s~%" test-inst))
             (core-run-test test-inst)))
         ;; (format t "      Exiting loop entry ~s~%" test)
             )
         ;;(format t "    Exiting run loop for ~s~%" group-inst)
       )
     nil))

(defgeneric core-run-test (test)
  (:documentation
   "Test fixtures provide name-binding :around methods to this generic function
for individual tests.  Every-test and test-specific setup and cleanup are
encoded as :before and :after methods.")

  (:method :around (test)
    "Capture the result of the test."
    (let ((*nst-group-name* (group-name test))
          (*nst-check-name* (check-name test))
          (start-time))
      (when (> *nst-verbosity* 1)
        (format t " - Executing test ~s~%" (check-name test)))
      (setf start-time (get-internal-real-time))
      (let ((result (call-next-method))
            (end-time (get-internal-real-time)))
        (setf (result-stats-elapsed-time result)
              (- end-time start-time)
              (gethash (canonical-storage-name (type-of test))
                       +results-record+)
              result)
        (when (> *nst-verbosity* 1)
          (format t "   ~s~%" result))
        result))))

;;;
;;; Programmatic starters for a test from Lisp.  Other starters such
;;; as via ASDF and vendor-specific REPL macros call these functions;
;;; from pure Lisp these are the top-level calls.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))

    ;; Print a message at the appropriate level of verbosity.
    (when (> *nst-verbosity* 0)
      (format t "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%"
        (package-name user-package) group-names))

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

;;; --------------------------------------------------------------
;;; This section of function definitions is not immediately in use,
;;; but I'm planning to shift over to them to eliminate (all but one)
;;; side package creation, and to (mostly) reduce class generation to
;;; one per fixture/class/test (the exception being 2nd class per
;;; fixture applied to a test).

(defun run-group-tests (group-obj)
  "Programmatic entry point for running all tests in a group."
  (do-group-prefixture-setup group-obj)
  (do-group-fixture-assignment group-obj)
  (do-group-afterfixture-cleanup group-obj))

(defgeneric do-group-prefixture-setup (group-obj)
  (:documentation
   "Pre-fixture group application setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-afterfixture-cleanup (group-obj)
  (:documentation
   "After-group fixture cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-fixture-assignment (group-obj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the group application class.")
  (:method progn (group-obj)
     (do-group-postfixture-setup group-obj)
     (do-group-test-iteration group-obj)
     (do-group-withfixture-cleanup group-obj)))

(defgeneric do-group-postfixture-setup (group-obj)
  (:documentation "Fixture setup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defgeneric do-group-withfixture-cleanup (group-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (group-obj) (declare (ignorable group-obj))))

(defun do-group-test-iteration (group-inst)
  ;; We might manually inline this function inside
  ;; do-group-fixture-assignment --- I think no one else will call it.

  (let ((group-name (group-name group-inst)))
    (when (> *nst-verbosity* 3)
      (format t "    Starting run loop for ~s~%" group-inst))
    (loop for test in (test-names group-inst) do
          (when (> *nst-verbosity* 3)
            (format t "      Starting loop entry ~s~%" test))
          (let ((in-suite-class-name (suite-class-name group-name test)))
            (when (> *nst-verbosity* 3)
              (format t "    Suite class name ~s~%" in-suite-class-name)
              (format t "    Actual class ~s~%"
                (find-class in-suite-class-name)))
            (let ((test-inst (make-instance in-suite-class-name)))
              (when (> *nst-verbosity* 3)
                (format t "    Instance ~s~%" test-inst))
              (do-test-main test-inst)))
          ;; (format t "      Exiting loop entry ~s~%" test)
          )
    ;;(format t "    Exiting run loop for ~s~%" group-inst)
    ))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun run-standalone-test (test-obj)
  "Programmatic entry point for running a standalone test."
  (do-standalone-pre-group-fixture-setup test-obj)
  (do-standalone-group-fixture-assignment test-obj)
  (do-standalone-after-group-fixture-cleanup test-obj))

(defgeneric do-standalone-pre-group-fixture-setup (testobj)
  (:documentation
   "Pre-fixture application setup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (testobj) (declare (ignorable testobj))))

(defgeneric do-standalone-after-group-fixture-cleanup (standalone-testobj)
  (:documentation
   "After-group fixture cleanup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (standalone-testobj) (declare (ignorable standalone-testobj))))

(defgeneric do-standalone-group-fixture-assignment (standalone-testobj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the group application class.")
  (:method (testobj)
     (do-standalone-with-group-fixture-setup testobj)
     (do-test-main testobj)
     (do-standalone-with-group-fixture-cleanup testobj)))

(defgeneric do-standalone-with-group-fixture-setup (testobj)
  (:documentation "Fixture setup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (standalone-testobj) (declare (ignorable standalone-testobj))))

(defgeneric do-standalone-with-group-fixture-cleanup (testobj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the group application class.")
  (:method-combination progn)
  (:method progn (testobj) (declare (ignorable testobj))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun do-test-main (test-inst)
  "Test execution gateway for both group and standalone execution."
  (do-test-prefixture-setup test-inst)
  (do-test-fixture-assignment test-inst)
  (do-test-afterfixture-cleanup test-inst))

(defgeneric do-test-prefixture-setup (test-obj)
  (:documentation
   "Pre-fixture test application setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-afterfixture-cleanup (test-obj)
  (:documentation
   "After-test fixture cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-fixture-assignment (test-obj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the test application class.")
  (:method (test-obj)
     (do-test-postfixture-setup test-obj)
     (core-run-test test-obj)
     (do-test-withfixture-cleanup test-obj)))

(defgeneric do-test-withfixture-cleanup (test-obj)
  (:documentation "Fixture setup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

(defgeneric do-test-postfixture-setup (test-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj) (declare (ignorable test-obj))))

;;; ------------------------------------------------------------

(defun run-group (group)
  "Run a group by its user-given name."
  (let ((group-class (group-class-name group)))

    ;; Print a message at the appropriate level of verbosity.
    (cond
      ((> *nst-verbosity* 3)
       (format t "Running group ~s --> ~s~%" group group-class))
      ((> *nst-verbosity* 0)
       (format t "Running group ~s~%" group)))

    (unless group-class (error 'no-such-nst-group :group group))
    (core-run (make-instance group-class))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name)."
  (let ((test-class (standalone-class-name group test)))

    ;; Print a message at the appropriate level of verbosity.
    (when (> *nst-verbosity* 0)
      (format t "Running test ~s (group ~s)~%" test group))

    (unless test-class (error 'no-such-nst-test :group group :test test))
    (core-run (make-instance test-class))))

;;;
;;; Management of global properties.
;;;

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
