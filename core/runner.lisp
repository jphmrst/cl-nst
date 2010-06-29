;;; File runner.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
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

;;; This file defines the functions which implement the main control
;;; flow of test and group execution.

;;; ----------------------------------------------------------------------
;;; Programmatic starters for a test from Lisp.  Starters such
;;; as via ASDF and REPL macros call these functions.
;;;
(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package.  Note that this is /not/ an interactive
function --- certain behaviors provided by e.g. the ASDF extension or
REPL macros require the dynamic configuration provided by those wrappers."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))
    (note-artifact-choice (package-name user-package) user-package)

    ;; Print a message at the appropriate level of verbosity.
    (format-at-verbosity 0 "~@<Running package ~s (groups ~{~s~^ ~:_~})~:>~%"
        (package-name user-package) group-names)

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

(defun run-group (group-class)
  "Run a group by its user-given name.    Note that this is /not/ an interactive
function --- certain behaviors provided by e.g. the ASDF extension or
REPL macros require the dynamic configuration provided by those wrappers."
  ;; Print a message at the appropriate level of verbosity.
  (format-at-verbosity 0 "Running group ~s~%" group-class)

  (unless group-class (error 'no-such-nst-group :group group-class))
  (run-group-inst (make-instance group-class)))

(defun run-group-inst (group-inst)
  (format-at-verbosity 4 "Called (run-group-inst ~s)~%" group-inst)
  (let ((test-lookups (test-name-lookup group-inst)))
    (note-artifact-choice (group-name group-inst) group-inst)
    (run-group-tests group-inst
                     (loop for test-name in (test-names group-inst)
                         collect (gethash test-name test-lookups)))))

(defun run-test-inst (test-inst)
  (format-at-verbosity 4 "Called (run-test-inst ~s)~%" test-inst)
  (let ((group-inst (make-instance (group-name test-inst))))
    (note-artifact-choice (test-name-lookup test-inst) test-inst)
    (run-group-tests group-inst (list test-inst))))

(defun run-test (group test)
  "Run a test standalone by its user-given name (and its group's name).
Note that this is /not/ an interactive function --- certain behaviors
provided by e.g. the ASDF extension or REPL macros require the dynamic
configuration provided by those wrappers."
  (format-at-verbosity 4 "Called (run-test ~s ~s)~%" group test)
  (let ((group-class (find-class group)))
    (unless group-class (error 'no-such-nst-group :group group))
    (let ((group-inst (make-instance group)))
      (let* ((test-lookups (test-name-lookup group-inst))
             (test-inst (gethash test test-lookups)))
        (unless test-inst (error 'no-such-nst-test :group group :test test))
        (note-artifact-choice (test-name-lookup test-inst) test-inst)

        ;; Print a message at the appropriate level of verbosity.
        (format-at-verbosity 0 "Running test ~s (group ~s)~%" test group)

        (run-group-tests group-inst (list test-inst))))))

;;; --------------------------------------------------------------

(defmacro with-retry ((continuation-label) &body forms)
  (let ((inner (gensym)) (outer (gensym)))
    `(block ,outer
       (loop do
         (block ,inner
           (handler-bind-interruptable
            ((debug-for-fail #'(lambda (e)
                                 (when *debug-on-fail*
                                   (cerror ,continuation-label e)
                                   (return-from ,inner))))
             (error #'(lambda (e)
                        (format-at-verbosity 4
                                             "In the retry handler ~s~%"
                                             ',continuation-label)
                        (when *debug-on-error*
                          (cerror ,continuation-label e)
                          (return-from ,inner)))))
            (return-from ,outer
              (progn ,@forms))))))))

(define-condition debug-for-fail (condition)
  ()
  (:documentation "Condition raised")
  (:report "Test failed."))

(defun run-group-tests (group-obj test-objs)
  "Programmatic entry point for running all tests in a group."
  (format-at-verbosity 4 "Called (run-group-tests ~s ~s)~%" group-obj test-objs)
  (with-retry ("Try performing group setup again.")
    (let ((exit-tests-label
           (format nil "~@<Exit from attempting tests in this group (~s), ~
                         ~_and continue with tests from other groups.~:>"
             (group-name group-obj))))
    (handler-bind-interruptable
     ((debug-for-fail #'(lambda (e)
                          (when *debug-on-fail*
                            (cerror exit-tests-label e)
                            (return-from run-group-tests nil))))
      (error #'(lambda (e)
                 (format-at-verbosity 4
                     "In the setup handler for run-group-tests ~s ~s~%"
                   group-obj test-objs)
                 (loop for test-obj in test-objs do
                       (setf (gethash (check-group-name test-obj)
                                      +results-record+)
                         (emit-config-error e test-obj
                           "Error in pre-fixture setup")))
                 (when *debug-on-error*
                   (cerror exit-tests-label e))
                 (return-from run-group-tests nil))))
     (do-group-prefixture-setup group-obj))))
  (format-at-verbosity 4
      "Passed setup in (run-group-tests ~s ~s)~%" group-obj test-objs)
  (with-retry
      ((format nil "Restart testing group ~s (reapplying group fixtures)"
         (group-name group-obj)))
    (block group-fixture-assignment
      (let ((exit-tests-label (format nil
                                  "~@<Skip group ~s (cleaning up first)~:>"
                                (group-name group-obj))))
      (handler-bind-interruptable
       ((debug-for-fail #'(lambda (e)
                            (when *debug-on-fail*
                              (cerror exit-tests-label e)
                              (return-from group-fixture-assignment nil))))
        (error #'(lambda (e)
                   (format-at-verbosity 4
                       "In the test handler for run-group-tests ~s ~s~%"
                     group-obj test-objs)
                   (loop for test-obj in test-objs do
                         (setf (gethash (check-group-name test-obj)
                                        +results-record+)
                           (emit-config-error e test-obj
                             (format nil "Error binding group fixture ~s"
                               *binding-variable*))))
                   (when *debug-on-error*
                     (cerror exit-tests-label e))
                   (return-from group-fixture-assignment nil))))
       (do-group-fixture-assignment group-obj test-objs)))))
  (format-at-verbosity 4
      "Passed test execution in (run-group-tests ~s ~s)~%" group-obj test-objs)
  (with-retry ("Try performing group cleanup again.")
    (let ((exit-tests-label "Continue with tests from other groups."))
      (handler-bind-interruptable
       ((debug-for-fail #'(lambda (e)
                            (when *debug-on-fail*
                              (cerror exit-tests-label e)
                              (return-from run-group-tests nil))))
        (error #'(lambda (e)
                   (format-at-verbosity 4
                       "In the cleanup handler for run-group-tests ~s ~s~%"
                     group-obj test-objs)
                   (loop for test-obj in test-objs do
                         (add-test-config-error test-obj
                           "Error in post-fixtures cleanup: ~s" e))
                   (when *debug-on-error*
                     (cerror exit-tests-label e))
                   (return-from run-group-tests nil))))
       (do-group-afterfixture-cleanup group-obj)))))

(defgeneric group-fixtures-setup-thunk (record))
(defun do-group-prefixture-setup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-prefixture-setup 'nst-group-record))
  (funcall (group-fixtures-setup-thunk record)))

(defgeneric group-fixtures-cleanup-thunk (record))
(defun do-group-afterfixture-cleanup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-afterfixture-cleanup 'nst-group-record))
  (funcall (group-fixtures-cleanup-thunk record)))

(defgeneric do-group-fixture-assignment (group-obj test-objs)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the group application class.")
  (:method (group-obj test-objs)
     (format-at-verbosity 4 "Called (do-group-fixture-assignment ~s ~s)~%"
       group-obj test-objs)
     (with-retry ("Try performing postfixture group setup again.")
       (let ((exit-tests-label (format nil
                                   "Skip group ~s (postfixture clean up first)"
                                 (group-name group-obj))))
       (handler-bind-interruptable
        ((debug-for-fail #'(lambda (e)
                             (when *debug-on-fail*
                               (cerror exit-tests-label e)
                               (return-from do-group-fixture-assignment nil))))
         (error
          #'(lambda (e)
              (format-at-verbosity 4
                  "In the postfixture setup handler for ~
                                        do-group-fixture-assignment ~s ~s~%"
                group-obj test-objs)
              (loop for test-obj in test-objs do
                    (setf (gethash (check-group-name test-obj)
                                   +results-record+)
                      (emit-config-error e test-obj
                        "Error in post-fixture application setup")))
              (when *debug-on-error*
                (cerror exit-tests-label e))
              (return-from do-group-fixture-assignment nil))))
        (do-group-postfixture-setup group-obj))))

     (format-at-verbosity 3 "    Starting run loop for ~s~%" group-obj)

     (unwind-protect
         (progn
           (with-retry ((format nil "Restart testing group ~s ~
                                               (not reapplying group fixtures)"
                          (group-name group-obj)))
             (loop for test-inst in test-objs do
               (unless test-inst (break "nil test"))
               (block this-test
                 (format-at-verbosity 3 "    Instance ~s~%" test-inst)
                 (with-retry ("Try each-test setup for this group again.")
                   (let ((exit-tests-label
                          (format nil "~@<Continue with other tests from ~
                                          this group ~_(~s, not likely to ~
                                          succeed).~:>"
                            (group-name group-obj))))
                   (handler-bind-interruptable
                       ((debug-for-fail #'(lambda (e)
                            (when *debug-on-fail*
                              (cerror exit-tests-label e)
                              (return-from this-test nil))))
                        (error
                         #'(lambda (e)
                             (format-at-verbosity 4
                                 "In the each-test setup handler for ~
                                  do-group-fixture-assignment ~s ~s~%"
                               group-obj test-objs)
                             (setf (gethash (check-group-name test-inst)
                                            +results-record+)
                               (emit-config-error
                                   e test-inst
                                 "Error in group each-test setup"))
                             (when *debug-on-error*
                               (cerror exit-tests-label e))
                             (return-from this-test nil))))
                     (do-group-each-test-setup group-obj))))
                 (unwind-protect
                     (block test-inner
                       (with-retry ((format nil "Try setting up test ~s again."
                                      test-inst))
                         (let ((exit-tests-label
                                (format nil "Skip this test (run the group's ~
                                             each-test cleanup, but not the ~
                                             test's cleanup)")))
                           (handler-bind-interruptable
                            ((debug-for-fail
                              #'(lambda (e)
                                  (when *debug-on-fail*
                                    (cerror exit-tests-label e)
                                    (return-from test-inner nil))))
                             (error
                              #'(lambda (e)
                                  (format-at-verbosity 4
                                      "In the prefixture setup handler for ~
                                        do-group-fixture-assignment ~s ~s~%"
                                    group-obj test-objs)
                                  (setf (gethash (check-group-name test-inst)
                                                 +results-record+)
                                    (emit-config-error e test-inst
                                      "Error in test pre-fixture setup"))
                                  (when *debug-on-error*
                                    (cerror exit-tests-label e))
                                  (return-from test-inner nil))))
                            (do-test-prefixture-setup test-inst))))
                       (unwind-protect
                           (with-retry ("Restart this test ~
                                                (reapplying test fixtures).")
                             (block test-fixture-assignment
                               (let ((exit-tests-label
                                      (format nil
                                          "Skip test ~s (running all cleanup)"
                                        (group-name group-obj))))
                                 (handler-bind-interruptable
                                  ((debug-for-fail
                                    #'(lambda (e)
                                        (when *debug-on-fail*
                                          (cerror exit-tests-label e)
                                          (return-from test-fixture-assignment
                                            nil))))
                                   (error
                                    #'(lambda (e)
                                        (format-at-verbosity 4
                                            "In the main test handler for ~
                                           do-group-fixture-assignment ~s ~s~%"
                                          group-obj test-objs)
                                        (setf (gethash (check-group-name
                                                        test-inst)
                                                       +results-record+)
                                          (emit-config-error
                                              e test-inst
                                            (format nil
                                                "Error binding test fixture ~s"
                                              *binding-variable*)))
                                        (when *debug-on-error*
                                          (cerror exit-tests-label e))
                                        (return-from test-fixture-assignment
                                          nil))))
                                  (do-test-fixture-assignment test-inst)))))
                         (with-retry ("Try test cleanup again.")
                           (let ((exit-tests-label
                                  (format nil "Continue with other tests from ~
                                               this group (~s)."
                                    (group-name group-obj))))
                           (handler-bind-interruptable
                               ((debug-for-fail
                                 #'(lambda (e)
                                     (when *debug-on-fail*
                                       (cerror exit-tests-label e)
                                       (return-from test-inner nil))))
                                (error
                                 #'(lambda (e)
                                     (format-at-verbosity 4
                                         "In the cleanup handler for ~
                                          do-group-fixture-assignment ~s ~s~%"
                                       group-obj test-objs)
                                     (add-test-config-error test-inst
                                       "Error in test postfixture cleanup: ~s"
                                       e)
                                     (when *debug-on-error*
                                       (cerror exit-tests-label e))
                                     (return-from test-inner))))
                             (do-test-afterfixture-cleanup test-inst))))))
                   (with-retry ("Try each-test cleanup again.")
                     (let ((exit-tests-label
                            (format nil
                                "~@<Continue with other tests from this ~
                                    group ~_(~s, this error likely to ~
                                    recur).~:>"
                              (group-name group-obj))))
                       (handler-bind-interruptable
                        ((debug-for-fail
                          #'(lambda (e)
                              (when *debug-on-fail*
                                (cerror exit-tests-label e)
                                (return-from this-test nil))))
                         (error
                          #'(lambda (e)
                              (format-at-verbosity 4
                                  "In the each-test cleanup handler for ~
                                    do-group-fixture-assignment ~s ~s~%"
                                group-obj test-objs)
                              (add-test-config-error test-inst
                                "Error in group each-test cleanup: ~s" e)
                              (when *debug-on-error*
                                (cerror exit-tests-label e))
                              (return-from this-test))))
                        (do-group-each-test-cleanup group-obj))))
                   (format-at-verbosity 3
                       "      Exiting loop entry ~s~%" test-inst)))))

           (format-at-verbosity 3
               "    Exiting run loop for ~s~%" group-obj))

       (with-retry ("Try performing group with-fixtures cleanup again.")
         (let ((exit-tests-label "Continue with tests from other groups."))
           (handler-bind-interruptable
            ((debug-for-fail
              #'(lambda (e)
                  (when *debug-on-fail*
                    (cerror exit-tests-label e)
                    (return-from do-group-fixture-assignment nil))))
             (error #'(lambda (e)
                        (format-at-verbosity 4
                            "In the group fixtures cleanup handler for ~
                              do-group-fixture-assignment ~s ~s~%"
                          group-obj test-objs)
                        (loop for test-obj in test-objs do
                              (add-test-config-error test-obj
                                "Error in group fixtures cleanup: ~s" e))
                        (when *debug-on-error*
                          (cerror exit-tests-label e))
                        (return-from do-group-fixture-assignment))))
            (do-group-withfixture-cleanup group-obj)))))))

(defgeneric group-withfixtures-setup-thunk (record))
(defun do-group-postfixture-setup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-postfixture-setup 'nst-group-record))
  (funcall (group-withfixtures-setup-thunk record)))

(defgeneric group-withfixtures-cleanup-thunk (record))
(defun do-group-withfixture-cleanup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-withfixture-cleanup 'nst-group-record))
  (funcall (group-withfixtures-cleanup-thunk record)))

(defgeneric group-eachtest-setup-thunk (record))
(defun do-group-each-test-setup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-each-test-setup 'nst-group-record))
  (funcall (group-eachtest-setup-thunk record)))

(defgeneric group-eachtest-cleanup-thunk (record))
(defun do-group-each-test-cleanup (record)
  (unless (typep record 'nst-group-record)
    (error "Called ~s with an argument not of type ~s"
           'do-group-each-test-cleanup 'nst-group-record))
  (funcall (group-eachtest-cleanup-thunk record)))

(defgeneric do-test-prefixture-setup (test-obj)
  (:documentation
   "Pre-fixture test application setup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj)
     (declare (ignorable test-obj))
     (format-at-verbosity 4
         "Called (do-test-prefixture-setup ~s) :progn primary method~%"
       test-obj)))

(defgeneric do-test-afterfixture-cleanup (test-obj)
  (:documentation
   "After-test fixture cleanup specs add a method to this function.")
  (:method-combination progn)
  (:method progn (test-obj)
     (declare (ignorable test-obj))
     (format-at-verbosity 4
         "Called (do-test-afterfixture-cleanup ~s) :progn primary method~%"
       test-obj)))

(defgeneric do-test-fixture-assignment (test-obj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the test application class.")
  (:method (test-obj)
     (format-at-verbosity 4 "Called (do-test-fixture-assignment ~s)~%"
       test-obj)
     (let ((exit-tests-label (format nil
                                 "Continue with other tests in this group (~s)"
                               (group-name test-obj))))
       (handler-bind-interruptable
        ((debug-for-fail
          #'(lambda (e)
              (when *debug-on-fail*
                (cerror exit-tests-label e)
                (return-from do-test-fixture-assignment nil))))
         (error
          #'(lambda (e)
              (format-at-verbosity 4
                  "In the setup handler for do-test-fixture-assignment ~s~%"
                test-obj)
              (setf (gethash (check-group-name test-obj)
                             +results-record+)
                (emit-config-error e test-obj
                  "Error in test post-fixture setup"))
              (when *debug-on-error*
                (cerror exit-tests-label e))
              (return-from do-test-fixture-assignment nil))))
        (do-test-postfixture-setup test-obj)))
     (unwind-protect (core-run-test test-obj)
       (let ((exit-tests-label
              (format nil "Continue with other tests in this group (~s)"
                (group-name test-obj))))
         (handler-bind-interruptable
          ((debug-for-fail
            #'(lambda (e)
                (when *debug-on-fail*
                  (cerror exit-tests-label e)
                  (return-from do-test-fixture-assignment nil))))
           (error #'(lambda (e)
                      (format-at-verbosity 4
                          "In the cleanup handler for ~
                                              do-test-fixture-assignment ~s~%"
                        test-obj)
                      (add-test-config-error test-obj
                        "Error in test fixtures cleanup: ~s" e)
                      (when *debug-on-error*
                        (cerror exit-tests-label e))
                      (return-from do-test-fixture-assignment nil))))
          (do-test-withfixture-cleanup test-obj))))))

(defgeneric do-test-postfixture-setup (test-obj)
  (:documentation "With-fixtures cleanup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj)
     (declare (ignorable test-obj))
     (format-at-verbosity 4
         "Called (do-test-postfixture-setup ~s) :progn primary method~%"
       test-obj)))

(defgeneric do-test-withfixture-cleanup (test-obj)
  (:documentation "Fixture setup specs add a method to this function
for the test application class.")
  (:method-combination progn)
  (:method progn (test-obj)
     (declare (ignorable test-obj))
     (format-at-verbosity 4
         "Called (do-test-withfixture-cleanup ~s) :progn primary method~%"
       test-obj)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defgeneric core-run-test (test)
  (:documentation
   "Test fixtures provide name-binding :around methods to this generic function
for individual tests.  Every-test and test-specific setup and cleanup are
encoded as :before and :after methods.")

  (:method :around (test)
    "Capture the result of the test."
    (format-at-verbosity 4 "Called (core-run-test ~s) common :around~%" test)
    (let ((*nst-group-name* (group-name test))
          (*nst-check-user-name* (test-name-lookup test))
          (*nst-check-internal-name* (check-group-name test))
          (start-time)
          (caught-warnings))
      (declare (special *nst-group-name* *nst-check-user-name*))
      (format-at-verbosity 1 " - Executing test ~s~%" *nst-check-user-name*)
      (setf start-time (get-internal-real-time))
      (let ((result (handler-bind ((warning
                                    #'(lambda (w)
                                        (push w caught-warnings)
                                        (muffle-warning w))))
                        (call-next-method)))
            (end-time (get-internal-real-time)))
        (setf (result-stats-elapsed-time result)
              (- end-time start-time)
              (gethash (check-group-name test) +results-record+)
              result)
        (when caught-warnings
          (setf (check-result-warnings result)
                (nconc (check-result-warnings result)
                       (loop for w in caught-warnings
                             collect (make-check-note
                                          :context *nst-context*
                                          :stack *nst-stack*
                                          :format "Lisp warning: ~
                                               ~:@_~/nst::format-for-warning/"
                                          :args (list w))))))
        (format-at-verbosity 1 "   ~s~%" result)
        (when (and *debug-on-fail* (or (check-result-errors result)
                                       (check-result-failures result)))
          (format-at-verbosity 4
              "Detected failure; triggering debug via error~%")
          (cerror "Cleanup and proceed." 'debug-for-fail))
        result))))

