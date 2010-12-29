;;; File runnable.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009-2010 Smart Information Flow Technologies.
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
(in-package :sift.asdf-nst)



(defclass nst-test-runner (system)
     ((nst-systems :initarg :nst-systems
                   :reader nst-systems
                   :initform nil
                   :documentation
                   "Other systems to which NST testing is delegated")

      (nst-init :initarg :nst-init
                :initform nil
                :reader nst-init
                :documentation
                "NST initialization steps.  Should a list of lists, each of
                 which gives arguments to run-nst-command/the REPL alias.")

      (nst-debug-config :initarg :nst-debug-config
                        :initform '(symbol-value (intern (symbol-name '*default-debug-config*)
                                                  :nst))
                        :reader nst-debug-config
                        :documentation
                        "NST debugging customization for this system.  Should be
                         an expression which, when evaluated, returns a list of
                         keyword arguments; see *nst-default-debug-config*.")
      (nst-debug-protect :initarg :nst-debug-protect
                         :initform nil
                         :reader nst-debug-protect
                         :documentation
                         "Globals to be saved/restored in an NST debug run.
                          List of elements (package . symbol)")
      (nst-push-debug-config :initarg :nst-push-debug-config
                             :initform nil
                             :reader nst-push-debug-config
                             :documentation
                             "If non-null, then when this system is loaded
                              its :nst-debug and :nst-debug-protect settings
                              will be used as NST's defaults."))

  (:documentation "Class of ASDF systems that use NST for their test-op."))

(defgeneric all-nst-testers (system)
  (:documentation "Returns three values:
1.  A set of PACKAGES,
2.  A set of NST GROUP NAMES
3.  A set of NST TEST specifiers (pairs)
that should be tested while testing SYSTEM.")
  (:method ((sym symbol))
     (let ((system (asdf:find-system sym)))
       (cond
        (system (get-test-artifacts system))
        (t (error "NST subsystem ~a not found" sym)))))

  (:method ((s nst-test-runner))
     (loop for subsystem in (nst-systems s)
           for (this-packages this-groups this-tests)
             = (multiple-value-list (get-test-artifacts subsystem))
           append this-packages into packages
           append this-groups into groups
           append this-tests into tests
           finally (return-from all-nst-testers
                     (values packages groups tests)))))

(defmethod asdf::component-depends-on :around ((op test-op)
                                               (sys nst-test-runner))
  "To test this system, we\'ll need to have loaded NST, and we\'ll need to have
loaded and tested the systems actually holding our unit tests.  There may be
other contributors to the to-do list, so we include those tasks via
\(call-next-method\)."
  `((asdf:load-op ,sys)                 ;note that ASDF 2 will make this a
                                        ;standard dependency.  But it's not a
                                        ;big deal to have an
                                        ;extra... [2010/10/25:rpg]
    (asdf:load-op :nst)
    ,@(loop for sub in (nst-systems sys)
            append `((asdf:load-op ,sub)
                     (asdf:test-op ,sub)))
    ,@(call-next-method)))

(defmethod all-nst-tested ((nst-test-runner nst-test-runner)
                           &optional
                           (all-packages (make-hash-table :test 'eq))
                           (all-groups (make-hash-table :test 'eq))
                           (all-tests-by-group (make-hash-table :test 'eq)))
  "Recursively assemble the artifacts identified by the systems we use."

  (with-accessors ((systems nst-systems)
                   (packages nst-packages) (package nst-package)
                   (group nst-group) (groups nst-groups)
                   (test nst-test) (tests nst-tests)) nst-test-runner

    ;; Grab symbols from subsystems.
    (loop for system in systems do
      (all-nst-tested (find-system system)
          all-packages all-groups all-tests-by-group))

    (values all-packages all-groups all-tests-by-group)))

(defmethod operation-done-p ((o asdf:test-op) (c nst-test-runner))
  "Always re-run NST for the test-op."
  (values nil))

(defmethod perform :after ((o asdf:load-op) (c nst-test-runner))
  "Run the NST initialization options for this system."
  (let ((options (nst-init c)))
    (loop for opt in options do
      (apply (symbol-function (intern (symbol-name 'run-nst-command)
                                      :nst))
             opt)))

  ;; Check whether we should export our debug configuration as NST's
  ;; defaults.  This allows the :run-test, :run-package, etc. commands
  ;; to pick up a system's debug configuration.
  (when (nst-push-debug-config c)
    (setf (symbol-value (intern (symbol-name '*default-debug-config*) :nst))
          (eval (nst-debug-config c))
          (symbol-value (intern (symbol-name '*default-debug-protect*) :nst))
          (nst-debug-protect c))))

(defmethod perform ((o asdf:test-op) (c nst-test-runner))
  (macrolet ((nst-fn (fn &rest args)
               `(funcall (symbol-function (intern (symbol-name ',fn) :nst))
                         ,@args)))

    ;; Check whether we're running this system on behalf of another
    ;; system.
    (unless *intermediate-system*

      ;; If not, report all the results from both this system, and
      ;; subordinated systems.
      (multiple-value-bind (package-set group-set test-set) (all-nst-tested c)
        (nst-fn report-multiple
                (loop for s being the hash-keys of package-set collect s)
                (loop for s being the hash-keys of group-set collect s)
                (loop for g being the hash-keys of test-set
                    using (hash-value hash)
                    append (loop for ts being the hash-keys of hash
                               collect (cons g ts)))
                :system c)))))
