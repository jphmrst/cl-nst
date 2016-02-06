;;; File runner.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Copyright (c) 2016 John Maraist
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :nst)

;;; This file defines the functions which implement the main control
;;; flow of test and group execution.

;;; ----------------------------------------------------------------------
;;; Programmatic starters for a test from Lisp.  Starters such
;;; as via ASDF and REPL macros call these functions.
;;;

;;; (defun run-multiple (packages groups tests)
;;;  (loop for group in groups collect

(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package.  Note that this is /not/ an interactive
function --- certain behaviors provided by e.g. the ASDF extension or
REPL macros require the dynamic configuration provided by those wrappers."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))
    (note-name-invocation user-package)
    ;; (note-artifact-choice (package-name user-package) user-package)

    ;; Print a message at the appropriate level of verbosity.
    (with-output-for-verbosity (0 verb)
      (pprint-logical-block (verb '(1 2))
        (format verb "Running package ~s" (package-name user-package))
        (with-output-for-verbosity (2 verb)
          (format verb " (groups ")
          (pprint-logical-block (verb group-names)
            (loop for (group-name . others) on group-names do
                  (format verb "~s" group-name)
                  (when others
                    (princ " " verb)
                    (pprint-newline :mandatory verb))))
          (format verb ")")))
      (format verb "~%"))

    ;; Run each group.
    (cond
      (group-names (loop for group-name in group-names
                         do (run-group group-name)))
      (t (error 'no-nst-groups-in-package :package package-or-name)))))

(defun run-group (group-name)
  "Run a group by its user-given name.    Note that this is /not/ an
interactive function --- certain behaviors provided by e.g. the ASDF extension
or REPL macros require the dynamic configuration provided by those wrappers."
  (let ((group-record (group-record group-name)))
    (unless group-record (error 'no-such-nst-group :group group-name))
    (run-group-record group-record)))

(defvar *implicit-group-choice* nil)
(defun run-group-record (group-record)

  ;; Print a message at the appropriate level of verbosity.
  (cond
    (*implicit-group-choice*
     (format-at-verbosity 1 "Also running included group ~s~%"
       (group-record-name group-record)))
    (t (format-at-verbosity 0 "Running group ~s~%"
                            (group-record-name group-record))))

  ;; Run the tests associated with the group
  (let ((test-lookups (group-record-tests group-record)))
    (unless *implicit-group-choice*
      ;; (note-artifact-choice (group-record-name group-record) group-record)
      (note-name-invocation group-record))
    (run-group-tests group-record
                     (loop for test-record being the hash-values of test-lookups
                           collect test-record)))

  (let ((*implicit-group-choice* t))
    (declare (special *implicit-group-choice*))
    (loop for included-group in (group-record-include-groups group-record) do
      (run-group included-group))))

(defun run-test-inst (test-record)
  (format-at-verbosity 4 "Called (run-test-inst ~s)~%" test-record)
  (let* ((group-record (test-record-group test-record)))
    ;; (note-artifact-choice (test-name-lookup test-inst) test-inst)
    (note-name-invocation test-record)
    (run-group-tests group-record (list test-record))))

(defun run-test (group-name test-name)
  "Run a test standalone by its user-given name (and its group's name).
Note that this is /not/ an interactive function --- certain behaviors
provided by e.g. the ASDF extension or REPL macros require the dynamic
configuration provided by those wrappers."

  ;; Print a message at the right verbosity.
  (format-at-verbosity 4 "Called (run-test ~s ~s)~%" group-name test-name)

  ;; Fetch the group and test records.
  (let ((group-record (group-record group-name))
        (test-record (test-record group-name test-name)))
    (unless group-record (error 'no-such-nst-group :group group-name))
    (unless test-record
      (error 'no-such-nst-test :group group-name :test test-name))

    (note-name-invocation test-record)

    ;; Print a message at the appropriate level of verbosity.
    (format-at-verbosity 0 "Running test ~s (group ~s)~%" test-name group-name)

    (run-group-tests group-record (list test-record))))

;;; --------------------------------------------------------------
(defun run-group-tests (group-record test-records)
  "Programmatic entry point for running all tests in a group."
  (let ((group-name (group-record-name group-record)))

    ;; Print a message at the right verbosity.
    (format-at-verbosity 4  "Called (run-group-tests ~s ~s)~%"
                         group-name (mapcar #'test-record-name test-records))

    ;; Run group pre-fixture setup.
    (let ((thunk (group-record-fixtures-setup-thunk group-record)))
      (when thunk
        (with-nst-control-handlers
            ((e flag
                :cerror-label (format nil
                                  "~@<Exit from attempting tests in this group ~
                                    ~_(~s), and continue with tests from other ~
                                 groups.~:>"
                                group-name)
                :with-retry "Try performing group setup again."
                :handler-return-to run-group-tests
                :group group-record :tests test-records
                :fail-test-msg "Error in pre-fixture setup"
                :log-location ("in setup handler" :group group-record
                                                  :tests test-records)))

          ;; The pre-fixture setup call wrapped by the above handlers.
          (funcall thunk))))
    (format-at-verbosity 4 "Passed setup in (run-group-tests ~s ~s)~%"
                         group-record test-records)

    ;; Apply fixtures and continue with group setup and test execution.
    (apply-fixtures (group-record-given-fixtures group-record)
                    group-record test-records
                    (run-group-tests-fixtures-thunk group-record test-records))
    (format-at-verbosity 4
        "Passed test execution in (run-group-tests ~s ~s)~%"
      group-record test-records)

    ;; Run group post-fixture cleanup.
    (let ((thunk (group-record-fixtures-cleanup-thunk group-record)))
      (when thunk
        (with-nst-control-handlers
            ((e flag
                :cerror-label "Continue with tests from other groups."
                :with-retry "Try performing group cleanup again."
                :handler-return-to run-group-tests
                :log-location ("in post-fixture cleanup"
                               :group group-record :tests test-records)
                :group group-record :tests test-records
                :fail-test-msg "Error in post-fixture cleanup"))
          (funcall thunk))))))

(defun run-group-tests-fixtures-thunk (group-record test-records)
  #'(lambda ()
      (block group-withfix
        ;; Run group post-fixture setup.
        (let ((thunk (group-record-withfixtures-setup-thunk group-record)))
          (when thunk
            (with-nst-control-handlers
                ((e flag
                    :cerror-label (format nil
                                      "~@<Exit from attempting tests in this ~
                                        group ~_(~s), and continue with tests ~
                                        from other groups.~:>"
                                    (group-record-name group-record))
                    :with-retry "Try performing post-fixture group setup again."
                    :handler-return-to group-withfix
                    :log-location ("in post-fixture setup"
                                   :group group-record :tests test-records)
                    :group group-record :tests test-records
                    :fail-test-msg "Error in post-fixture setup"))

              ;; The pre-fixture setup call wrapped by the above handlers.
              (funcall thunk))))
        (format-at-verbosity 4
            "Passed post-fixture setup in (run-group-tests ~s ~s)~%"
          group-record test-records)

        (format-at-verbosity 3 "    Starting run loop for ~s~%" group-record)
        (unwind-protect
            (progn
              (with-retry ((format nil "Restart testing group ~s ~
                                               (not reapplying group fixtures)"
                             (group-record-name group-record)))
                (loop for test-record in test-records do
                      (unless test-record (break "nil test"))
                      (run-group-test group-record test-record))))

          ;; Run group post-fixture cleanup, in the protected block of
          ;; the unwind-protect above.
          (let ((thunk (group-record-withfixtures-cleanup-thunk group-record)))
            (when thunk
              (with-nst-control-handlers
                  ((e flag
                      :cerror-label "Continue with tests from other groups."
                      :with-retry "Try performing group with-fixtures ~
                                 cleanup again."
                      :handler-return-to group-withfix
                      :log-location ("in group fixtures cleanup"
                                     :group group-record :tests test-records)
                      :group group-record :tests test-records
                      :fail-test-msg "Error in group fixtures cleanup"))
                (funcall thunk))))))))

(defun run-group-test (group-record test-record)
  ;; Run group each-test setup.
  (let ((thunk (group-record-eachtest-setup-thunk group-record)))
    (when thunk
      (with-nst-control-handlers
          ((e flag
              :cerror-label (format nil
                                "~@<Exit from this test, and continue with other ~
                                  tests in this group.~:>"
                              (group-record-name group-record))
              :with-retry "Try performing each-test setup again."
              :handler-return-to run-group-test
              :log-location ("in each-test setup"
                             :group group-record :test test-record)
              :group group-record :tests (list test-record)
              :fail-test-msg "Error in each-test setup"))
        (funcall thunk))))
  (format-at-verbosity 4 "Passed each-test setup for group ~a, test ~a~%"
                       (group-record-name group-record)
                       (test-record-name test-record))


  (unwind-protect
      (progn

        (let ((thunk (test-record-startup test-record)))
          (when thunk
            (with-nst-control-handlers
                ((e flag
                    :cerror-label "Retry test startup."
                    :with-retry "Retry test startup."
                    :handler-return-to run-group-test
                    :log-location ("in test startup"
                                   :group group-record :test test-record)
                    :group group-record :tests (list test-record)
                    :fail-test-msg "Error in test startup"))
              (funcall thunk))))
        (format-at-verbosity 4 "Passed test startup for group ~a, test ~a~%"
                             (group-record-name group-record)
                             (test-record-name test-record))


        (unwind-protect
            (apply-fixtures (test-record-fixtures test-record)
                            group-record (list test-record)
                            (run-test-fixtures-thunk group-record test-record))

          (let ((thunk (test-record-finish test-record)))
            (when thunk
              (with-nst-control-handlers
                  ((e flag
                      :cerror-label "~@<Exit from this test, and continue with ~
                                      other tests in this group.~:>"
                      :with-retry "Try performing test finish again."
                      :handler-return-to run-group-test
                      :log-location ("in test finish"
                                     :group group-record :test test-record)
                      :group group-record :tests (list test-record)
                      :fail-test-msg "Error in test finish"))
                (funcall thunk))))))

    (let ((thunk (group-record-eachtest-cleanup-thunk group-record)))
      (when thunk
        (with-nst-control-handlers
            ((e flag
                :cerror-label (format nil
                                  "~@<Exit from this test, and continue with ~
                                    other tests in this group.~:>"
                                (group-record-name group-record))
                :with-retry "Try performing each-test cleanup again."
                :handler-return-to run-group-test
                :log-location ("in each-test cleanup"
                               :group group-record :test test-record)
                :group group-record :tests (list test-record)
                :fail-test-msg "Error in each-test cleanup"))
          (funcall thunk))))))

(defun run-test-fixtures-thunk (group-record test-record)
  #'(lambda ()
      (block run-test-fixtures-thunk
        (let ((thunk (test-record-setup test-record)))
          (when thunk
            (with-nst-control-handlers
                ((e flag
                    :cerror-label (format nil
                                      "~@<Exit from this test, and continue with ~
                                    other tests in this group.~:>"
                                    (group-record-name group-record))
                    :with-retry "Try performing test setup again."
                    :handler-return-to run-test-fixtures-thunk
                    :log-location ("in test setup"
                                   :group group-record :test test-record)
                    :group group-record :tests (list test-record)
                    :fail-test-msg "Error in test setup"))
              (funcall thunk))))
        (format-at-verbosity 4 "Passed test setup for group ~a, test ~a~%"
                             (group-record-name group-record)
                             (test-record-name test-record))

        (unwind-protect
            (core-run-test test-record) ;; ACTUALLY RUNNING TEST HERE

          (let ((thunk (test-record-cleanup test-record)))
            (when thunk
              (with-nst-control-handlers
                  ((e flag
                      :cerror-label (format nil
                                        "~@<Exit from this test, and continue ~
                                          with other tests in this group.~:>"
                                      (group-record-name group-record))
                      :with-retry "Try performing test cleanup again."
                      :handler-return-to run-test-fixtures-thunk
                      :log-location ("in test cleanup"
                                     :group group-record :test test-record)
                      :group group-record :tests (list test-record)
                      :fail-test-msg "Error in test cleanup"))
                (funcall thunk))))))))

;;; =================================================================
;;; OLD STUFF BELOW
;;; =================================================================

;;;(defgeneric do-group-fixture-assignment (group-obj test-objs)
;;;  (:documentation
;;;   "Fixture declarations translate to an :around method making let* bindings
;;;for the group application class.")
;;;  (:method (group-obj test-objs)
;;;     (format-at-verbosity 4
;;;         "Called do-group-fixture-assignment principal on ~s ~s~%"
;;;       group-obj test-objs)
;;;     (with-nst-control-handlers
;;;         ((e flag
;;;             :cerror-label (format nil
;;;                               "Skip group ~s (postfixture clean up first)"
;;;                             (group-record-name group-obj))
;;;             :with-retry "Try performing postfixture group setup again."
;;;             :handler-return-to do-group-fixture-assignment
;;;             :log-location ("in postfixture group setup"
;;;                                :group group-obj :tests test-obj)
;;;          :group group-record :tests test-objs
;;;          :fail-test-msg "Error in post-fixture group setup"))
;;;       (funcall (group-withfixtures-setup-thunk group-obj)))
;;;
;;;     (format-at-verbosity 3 "    Starting run loop for ~s~%" group-obj)
;;;
;;;     (unwind-protect
;;;         (progn
;;;           (with-retry ((format nil "Restart testing group ~s ~
;;;                                               (not reapplying group fixtures)"
;;;                          (group-record-name group-obj)))
;;;             (loop for test-inst in test-objs do
;;;               (unless test-inst (break "nil test"))
;;;               (block this-test
;;;                 (format-at-verbosity 3 "    Instance ~s~%" test-inst)
;;;                 (with-nst-control-handlers
;;;                     ((e flag
;;;                         :cerror-label
;;;                         (format nil "~@<Continue with other tests from ~
;;;                                          this group ~_(~s, not likely to ~
;;;                                          succeed).~:>"
;;;                           (group-record-name group-obj))
;;;                         :with-retry
;;;                         "Try each-test setup for this group again."
;;;                         :handler-return-to this-test
;;;                         :log-location ("in each-test setup"
;;;                                            :group group-obj :test test-inst))
;;;                      (setf (gethash (check-group-name test-inst)
;;;                                     +results-record+)
;;;                        (make-config-error e test-inst
;;;                          "Error in group each-test setup")))
;;;                   (funcall (group-eachtest-setup-thunk group-obj)))
;;;                 (unwind-protect
;;;                     (block test-inner
;;;                       (with-nst-control-handlers
;;;                           ((e flag
;;;                               :cerror-label
;;;                               (format nil "Skip this test (run the group's ~
;;;                                             each-test cleanup, but not the ~
;;;                                             test's cleanup)")
;;;                               :with-retry
;;;                               (format nil "Try setting up test ~s again."
;;;                                 test-inst)
;;;                               :handler-return-to test-inner
;;;                               :log-location ("in prefixture setup"
;;;                                                  :group group-obj
;;;                               :group group-record :test test-inst))
;;;                            (setf (gethash (check-group-name test-inst)
;;;                                           +results-record+)
;;;                              (make-config-error e test-inst
;;;                                "Error in test pre-fixture setup")))
;;;                         (funcall (test-startup-form test-inst)))
;;;                       (unwind-protect
;;;                           (block test-fixture-assignment
;;;                             (with-nst-control-handlers
;;;                                 ((e flag :for-fail t
;;;                                     :cerror-label
;;;                                     (format nil
;;;                                         "Skip test ~s (running all cleanup)"
;;;                                       (group-name group-obj))
;;;                                     :with-retry "Restart this test ~
;;;                                                  (reapplying test fixtures)."
;;;                                     :handler-return-to
;;;                                     test-fixture-assignment
;;;                                     :log-location ("in main test handler"
;;;                                                        :group group-obj
;;;                                                        :test test-inst))
;;;                                  (setf (gethash (check-group-name
;;;                                                  test-inst)
;;;                                                 +results-record+)
;;;                                    (make-config-error e test-inst
;;;                                      (format nil
;;;                                          "Error binding test fixture ~s"
;;;                                        *binding-variable*))))
;;;                               (do-test-fixture-assignment test-inst)))
;;;                         (with-nst-control-handlers
;;;                             ((e flag
;;;                                 :cerror-label
;;;                                 (format nil "Continue with other tests ~
;;;                                                from this group (~s)."
;;;                                   (group-name group-obj))
;;;                                 :with-retry "Try test cleanup again."
;;;                                 :handler-return-to test-inner
;;;                                 :log-location ("in cleanup handler"
;;;                                                    :group group-obj
;;;                                                    :test test-inst))
;;;                              (add-test-config-error test-inst
;;;                                "Error in test postfixture cleanup: ~s" e))
;;;                           (funcall
;;;                            (test-finish-form test-inst)))))
;;;                   (with-nst-control-handlers
;;;                       ((e flag
;;;                           :cerror-label
;;;                           (format nil
;;;                               "~@<Continue with other tests from this ~
;;;                                    group ~_(~s, this error likely to ~
;;;                                    recur).~:>"
;;;                             (group-name group-obj))
;;;                           :with-retry "Try each-test cleanup again."
;;;                           :handler-return-to this-test
;;;                           :log-location ("in each-test cleanup"
;;;                                              :group group-obj
;;;                                              :test test-inst))
;;;                        (add-test-config-error test-inst
;;;                          "Error in group each-test cleanup: ~s" e))
;;;                     (funcall (group-eachtest-cleanup-thunk group-obj)))
;;;                   (format-at-verbosity 3
;;;                       "      Exiting loop entry ~s~%" test-inst)))))
;;;
;;;           (format-at-verbosity 3
;;;               "    Exiting run loop for ~s~%" group-obj))
;;;
;;;;;;       (with-nst-control-handlers
;;;;;;           ((e flag
;;;;;;               :cerror-label "Continue with tests from other groups."
;;;;;;               :with-retry "Try performing group with-fixtures cleanup again."
;;;;;;               :handler-return-to do-group-fixture-assignment)
;;;;;;            (format-at-verbosity 4
;;;;;;                "In the group fixtures cleanup handler for ~
;;;;;;                              do-group-fixture-assignment ~s ~s~%"
;;;;;;              group-obj test-objs)
;;;;;;            (loop for test-obj in test-objs do
;;;;;;                  (add-test-config-error test-obj
;;;;;;                    "Error in group fixtures cleanup: ~s" e)))
;;;;;;         (funcall (group-withfixtures-cleanup-thunk group-obj)))
;;;       )))

;;;(defgeneric group-withfixtures-setup-thunk (record))
;;;
;;;(defgeneric group-withfixtures-cleanup-thunk (record))
;;;
;;;(defgeneric group-eachtest-setup-thunk (record))
;;;
;;;(defgeneric group-eachtest-cleanup-thunk (record))
;;;
;;;(defgeneric do-test-fixture-assignment (test-obj)
;;;  (:documentation
;;;   "Fixture declarations translate to an :around method making let* bindings
;;;for the test application class.")
;;;  (:method (test-obj)
;;;     (format-at-verbosity 4 "Called do-test-fixture-assignment principal method ~s~%"
;;;       test-obj)
;;;     (with-nst-control-handlers
;;;         ((e flag
;;;             :cerror-label (format nil
;;;                               "Continue with other tests in this group (~s)"
;;;                             (group-name test-obj))
;;;             :handler-return-to do-test-fixture-assignment)
;;;          (format-at-verbosity 4
;;;                               "Setup handler for do-test-fixture-assignment ~s~%"
;;;            test-obj)
;;;          (setf (gethash (check-group-name test-obj) +results-record+)
;;;            (make-config-error e test-obj
;;;                               "Error in test post-fixture setup")))
;;;       (funcall (test-setup-form test-obj)))
;;;     (unwind-protect (core-run-test test-obj)
;;;       (with-nst-control-handlers
;;;           ((e flag
;;;               :cerror-label (format nil
;;;                                 "Continue with other tests in this group (~s)"
;;;                               (group-name test-obj))
;;;               :handler-return-to do-test-fixture-assignment)
;;;            (format-at-verbosity 4
;;;                "In the cleanup handler for do-test-fixture-assignment ~s~%"
;;;              test-obj)
;;;            (add-test-config-error test-obj
;;;              "Error in test fixtures cleanup: ~s" e))
;;;         (funcall (test-cleanup-form test-obj))))))

(defun wrap-fixture-name-specials (fixture-names-special form)
  (cond
   ((null fixture-names-special) form)
   ((equal fixture-names-special '(special)) form)
   ((and (listp fixture-names-special)
         (not (eq 'special (car fixture-names-special))))
    `(locally (declare (special ,@fixture-names-special)) ,form))
   (t `(locally (declare ,fixture-names-special) ,form))))

(defun get-test-record-specials (test-record)
  (apply #'append
         (mapcar #'fixture-bindings
                 (append (group-record-given-fixtures
                          (test-record-group test-record))
                         (test-record-fixtures test-record)))))

(defun core-run-test (test)
  "Capture the result of the test."
  (declare (optimize (debug 3)))
  (format-at-verbosity 4 "Called (core-run-test ~s) common :around~%" test)
  (let ((*nst-group-name* (group-record-name (test-record-group test)))
        (*nst-check-user-name* (test-record-name test))
        ;; (*nst-check-internal-name* (check-group-name test))
        (start-time) (caught-warnings))
    (declare (special *nst-group-name* *nst-check-user-name*))
    (format-at-verbosity 1 " - Executing test ~s~%" *nst-check-user-name*)
    (setf start-time (get-internal-real-time))
    (let ((result (handler-bind ((warning
                                  (named-function core-run-test-warning-handler
                                    (lambda (w)
                                      (push w caught-warnings)
                                      (muffle-warning w)))))

                    (let ((forms (test-record-forms test))
                          (fixture-names-special
                           (get-test-record-specials test))
                          (criterion (test-record-criterion test)))
                      (with-retry ((format nil "Try running test ~s again"
                                     *nst-check-user-name*))
                        (cond
                         ((eql 1 (length forms))
                          (check-criterion-on-form
                           criterion
                           `(common-lisp:multiple-value-list
                                ,(wrap-fixture-name-specials
                                  fixture-names-special (car forms)))))
                         (t (check-criterion-on-form
                             criterion
                             (wrap-fixture-name-specials fixture-names-special
                                                         `(list ,@forms)))))))))
          (end-time (get-internal-real-time)))
      (setf (result-stats-elapsed-time result) (- end-time start-time)
            (gethash (test-record-results test) +results-record+) result)
      (when caught-warnings
        (setf (check-result-warnings result)
              (nconc (check-result-warnings result)
                     (mapcar #'wrap-thrown-lisp-warning caught-warnings))))
      (format-at-verbosity 1 "   ~s~%" result)
      (when (and *debug-on-fail* (or (check-result-errors result)
                                     (check-result-failures result)))
        (format-at-verbosity 4 "Detected failure; triggering debug via error~%")
        (cerror "Cleanup and proceed." 'debug-for-fail))
      result)))
