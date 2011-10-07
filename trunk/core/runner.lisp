;;; File runner.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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

;;; (defun run-multiple (packages groups tests)
;;;  (loop for group in groups collect

(defun run-package (&optional (package-or-name *package*))
  "Run all groups in a package.  Note that this is /not/ an interactive
function --- certain behaviors provided by e.g. the ASDF extension or
REPL macros require the dynamic configuration provided by those wrappers."
  (let* ((user-package (find-package package-or-name))
         (group-names (package-groups user-package)))
    (note-name-use-invocation (package-name user-package) :package)
    ;; (note-artifact-choice (package-name user-package) user-package)

    ;; Print a message at the appropriate level of verbosity.
    (with-output-for-verbosity (0 verb)
      (pprint-logical-block (verb '(1 2))
        (format verb "Running package ~s (groups " (package-name user-package))
        (loop for (group-name . others) on group-names do
          (format verb "~s" group-name)
          (when others
            (princ " " verb)
            (pprint-newline :mandatory verb)))
        (format verb ")"))
      (format verb "~%"))

    (cond
      (group-names
       (loop for group-name in group-names do (run-group group-name)))
      (t
       (error 'no-nst-groups-in-package :package package-or-name)))))

(defvar *implicit-group-choice* nil)
(defun run-group (group-class)
  "Run a group by its user-given name.    Note that this is /not/ an
interactive function --- certain behaviors provided by e.g. the ASDF extension
or REPL macros require the dynamic configuration provided by those wrappers."
  ;; Print a message at the appropriate level of verbosity.

  (unless group-class (error 'no-such-nst-group :group group-class))
  (run-group-inst (make-instance group-class)))

(defun run-group-inst (group-inst)
  (cond
    (*implicit-group-choice*
     (format-at-verbosity 1 "Also running included group ~s~%"
       (group-name group-inst)))
    (t (format-at-verbosity 0 "Running group ~s~%"
         (group-name group-inst))))
  (let ((test-lookups (test-name-lookup group-inst)))
    (unless *implicit-group-choice*
      ;; (note-artifact-choice (group-name group-inst) group-inst)
      (note-name-use-invocation (group-name group-inst) :group))
    (run-group-tests group-inst
                     (loop for test-name in (test-names group-inst)
                           collect (make-instance (gethash test-name
                                                           test-lookups)))))
  (let ((*implicit-group-choice* t))
    (declare (special *implicit-group-choice*))
    (loop for included-group in (group-include-groups group-inst) do
      (run-group included-group))))

(defun run-test-inst (test-inst)
  (format-at-verbosity 4 "Called (run-test-inst ~s)~%" test-inst)
  (let* ((group-name (group-name test-inst))
         (group-inst (make-instance group-name)))
    ;; (note-artifact-choice (test-name-lookup test-inst) test-inst)
    (note-name-use-invocation (test-name-lookup test-inst) :test group-name)
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
        (setf test-inst (make-instance test-inst))
        ;; (note-artifact-choice (test-name-lookup test-inst) test-inst)
        (note-name-use-invocation (test-name-lookup test-inst) :test group)

        ;; Print a message at the appropriate level of verbosity.
        (format-at-verbosity 0 "Running test ~s (group ~s)~%" test group)

        (run-group-tests group-inst (list test-inst))))))

;;; --------------------------------------------------------------

(defmacro with-nst-control-handlers
    (((formal flag &key
              (for-fail nil) (for-error t)
              (cerror-label-var nil cerror-label-var-supp-p)
              (cerror-label nil cerror-label-supp-p)
              (with-retry nil with-retry-supp-p)
              (cerror-by-flag t)
              (post-cerror nil post-cerror-supp-p)
              (handler-return-to nil handler-return-to-supp-p)
              (handler-return nil))
      &body handler)
     &body body)
  (when (and cerror-by-flag (not cerror-label-var-supp-p))
    (error
     "If providing :cerror-by-flag, must also provide :cerror-label-var"))
  (when (and post-cerror-supp-p (not cerror-by-flag))
    (error "Provided :post-cerror but not :cerror-by-flag"))
  (let* ((handler-body-fn (gensym "handler-body"))
         (core
         `(flet ((,handler-body-fn (,formal ,flag)
                   ,@handler
                   ,@(when cerror-by-flag
                       `((when ,flag
                           (cerror ,cerror-label-var ,formal)
                           ,@(when post-cerror-supp-p `(,post-cerror)))))
                   ,@(when handler-return-to-supp-p
                       `((return-from ,handler-return-to ,handler-return)))))
            (handler-bind-interruptable
                (,@(when for-fail
                     `((debug-for-fail
                        (named-function nst-control-for-fail
                          (lambda (,formal)
                            (funcall #',handler-body-fn
                                     ,formal *debug-on-fail*))))))
                 ,@(when for-error
                     `((error
                        (named-function nst-control-for-error
                          (lambda (,formal)
                            (funcall #',handler-body-fn
                                     ,formal *debug-on-error*)))))))
              ,@body))))

    (cond
     ((and cerror-label-supp-p cerror-label-var-supp-p)
      (setf core `(let ((,cerror-label-var ,cerror-label)) ,core)))
     ((and cerror-label-supp-p (not cerror-label-var-supp-p))
      (error "Gave :cerror-label but not :cerror-label-var")))

    (when with-retry-supp-p
      (setf core `(with-retry (,with-retry) ,core)))

    core))

(defmacro with-retry ((continuation-label) &body forms)
  (let ((inner (gensym)) (outer (gensym)))
    `(block ,outer
       (loop do
         (block ,inner
           (with-nst-control-handlers
               ((e flag
                   :for-fail t
                   :cerror-label-var ,continuation-label
                   :post-cerror (return-from ,inner))
                (format-at-verbosity 4 "In the retry handler ~s~%"
                  ',continuation-label))
             (return-from ,outer (progn ,@forms))))))))

(define-condition debug-for-fail (condition)
  ()
  (:documentation "Condition raised")
  (:report "Test failed."))

(defun run-group-tests (group-obj test-objs)
  "Programmatic entry point for running all tests in a group."
  (format-at-verbosity 4
      "Called (run-group-tests ~s ~s)~%" group-obj test-objs)
  (with-nst-control-handlers
      ((e flag
          :cerror-label-var exit-tests-label
          :cerror-label (format nil
                            "~@<Exit from attempting tests in this group ~
                              ~_(~s), and continue with tests from other ~
                              groups.~:>"
                          (group-name group-obj))
          :with-retry "Try performing group setup again."
          :handler-return-to run-group-tests)
       (format-at-verbosity 4
           "In the setup handler for run-group-tests ~s ~s~%"
         group-obj test-objs)
       (loop for test-obj in test-objs do
         (setf (gethash (check-group-name test-obj) +results-record+)
               (make-config-error e test-obj "Error in pre-fixture setup"))))
    (funcall (group-fixtures-setup-thunk group-obj)))
  (format-at-verbosity 4
      "Passed setup in (run-group-tests ~s ~s)~%" group-obj test-objs)
  (block group-fixture-assignment
    (with-nst-control-handlers
        ((e flag
            :for-fail t
            :cerror-label-var exit-tests-label
            :cerror-label (format nil "Skip group ~s (cleaning up first)"
                            (group-name group-obj))
            :with-retry
            (format nil "Restart testing group ~s (reapplying group fixtures)"
              (group-name group-obj))
            :handler-return-to group-fixture-assignment)
         (format-at-verbosity 4
             "In the test handler for run-group-tests ~s ~s~%"
           group-obj test-objs)
         (loop for test-obj in test-objs do
               (setf (gethash (check-group-name test-obj) +results-record+)
                 (make-config-error e test-obj
                   (format nil "Error binding group fixture ~s"
                     *binding-variable*)))))
      (do-group-fixture-assignment group-obj test-objs)))

  (format-at-verbosity 4
      "Passed test execution in (run-group-tests ~s ~s)~%" group-obj test-objs)
  (with-nst-control-handlers
      ((e flag
          :cerror-label-var exit-tests-label
          :cerror-label "Continue with tests from other groups."
          :with-retry "Try performing group cleanup again."
          :handler-return-to run-group-tests)
       (format-at-verbosity 4
           "In the cleanup handler for run-group-tests ~s ~s~%"
         group-obj test-objs)
       (loop for test-obj in test-objs do
             (add-test-config-error test-obj
               "Error in post-fixtures cleanup: ~s" e)))
    (funcall (group-fixtures-cleanup-thunk group-obj))))

(defgeneric group-fixtures-setup-thunk (record))
(defgeneric group-fixtures-cleanup-thunk (record))

(defgeneric do-group-fixture-assignment (group-obj test-objs)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the group application class.")
  (:method (group-obj test-objs)
     (format-at-verbosity 4
         "Called do-group-fixture-assignment principal on ~s ~s~%"
       group-obj test-objs)
     (with-nst-control-handlers
         ((e flag
             :cerror-label-var exit-tests-label
             :cerror-label (format nil
                               "Skip group ~s (postfixture clean up first)"
                             (group-name group-obj))
             :with-retry "Try performing postfixture group setup again."
             :handler-return-to do-group-fixture-assignment)
          (format-at-verbosity 4
              "In the postfixture setup handler for ~
                   do-group-fixture-assignment ~s ~s~%" group-obj test-objs)
          (loop for test-obj in test-objs do
                (setf (gethash (check-group-name test-obj) +results-record+)
                  (make-config-error e test-obj
                    "Error in post-fixture application setup"))))
       (funcall (group-withfixtures-setup-thunk group-obj)))

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
                 (with-nst-control-handlers
                     ((e flag
                         :cerror-label-var exit-tests-label
                         :cerror-label
                         (format nil "~@<Continue with other tests from ~
                                          this group ~_(~s, not likely to ~
                                          succeed).~:>"
                           (group-name group-obj))
                         :with-retry
                         "Try each-test setup for this group again."
                         :handler-return-to this-test)
                      (format-at-verbosity 4
                          "In the each-test setup handler for ~
                                  do-group-fixture-assignment ~s ~s~%"
                        group-obj test-objs)
                      (setf (gethash (check-group-name test-inst)
                                     +results-record+)
                        (make-config-error e test-inst
                          "Error in group each-test setup")))
                   (funcall (group-eachtest-setup-thunk group-obj)))
                 (unwind-protect
                     (block test-inner
                       (with-nst-control-handlers
                           ((e flag
                               :cerror-label-var exit-tests-label
                               :cerror-label
                               (format nil "Skip this test (run the group's ~
                                             each-test cleanup, but not the ~
                                             test's cleanup)")
                               :with-retry
                               (format nil "Try setting up test ~s again."
                                 test-inst)
                               :handler-return-to test-inner)
                            (format-at-verbosity 4
                                "In the prefixture setup handler for ~
                                     do-group-fixture-assignment ~s ~s~%"
                              group-obj test-objs)
                            (setf (gethash (check-group-name test-inst)
                                           +results-record+)
                              (make-config-error e test-inst
                                "Error in test pre-fixture setup")))
                         (funcall (test-startup-form test-inst)))
                       (unwind-protect
                           (block test-fixture-assignment
                             (with-nst-control-handlers
                                 ((e flag :for-fail t
                                     :cerror-label-var exit-tests-label
                                     :cerror-label
                                     (format nil
                                         "Skip test ~s (running all cleanup)"
                                       (group-name group-obj))
                                     :with-retry "Restart this test ~
                                                  (reapplying test fixtures)."
                                     :handler-return-to
                                     test-fixture-assignment)
                                  (format-at-verbosity 4
                                      "In the main test handler for ~
                                           do-group-fixture-assignment ~s ~s~%"
                                    group-obj test-objs)
                                  (setf (gethash (check-group-name
                                                  test-inst)
                                                 +results-record+)
                                    (make-config-error e test-inst
                                      (format nil
                                          "Error binding test fixture ~s"
                                        *binding-variable*))))
                               (do-test-fixture-assignment test-inst)))
                         (with-nst-control-handlers
                             ((e flag
                                 :cerror-label-var exit-tests-label
                                 :cerror-label
                                 (format nil "Continue with other tests ~
                                                from this group (~s)."
                                   (group-name group-obj))
                                 :with-retry "Try test cleanup again."
                                 :handler-return-to test-inner)
                              (format-at-verbosity 4
                                  "In the cleanup handler for ~
                                       do-group-fixture-assignment ~s ~s~%"
                                group-obj test-objs)
                              (add-test-config-error test-inst
                                "Error in test postfixture cleanup: ~s" e))
                           (funcall
                            (test-finish-form test-inst)))))
                   (with-nst-control-handlers
                       ((e flag
                           :cerror-label-var exit-tests-label
                           :cerror-label
                           (format nil
                               "~@<Continue with other tests from this ~
                                    group ~_(~s, this error likely to ~
                                    recur).~:>"
                             (group-name group-obj))
                           :with-retry "Try each-test cleanup again."
                           :handler-return-to this-test)
                        (format-at-verbosity 4
                            "In the each-test cleanup handler for ~
                                    do-group-fixture-assignment ~s ~s~%"
                          group-obj test-objs)
                        (add-test-config-error test-inst
                          "Error in group each-test cleanup: ~s" e))
                     (funcall (group-eachtest-cleanup-thunk group-obj)))
                   (format-at-verbosity 3
                       "      Exiting loop entry ~s~%" test-inst)))))

           (format-at-verbosity 3
               "    Exiting run loop for ~s~%" group-obj))

       (with-nst-control-handlers
           ((e flag
               :cerror-label-var exit-tests-label
               :cerror-label "Continue with tests from other groups."
               :with-retry "Try performing group with-fixtures cleanup again."
               :handler-return-to do-group-fixture-assignment)
            (format-at-verbosity 4
                "In the group fixtures cleanup handler for ~
                              do-group-fixture-assignment ~s ~s~%"
              group-obj test-objs)
            (loop for test-obj in test-objs do
                  (add-test-config-error test-obj
                    "Error in group fixtures cleanup: ~s" e)))
         (funcall (group-withfixtures-cleanup-thunk group-obj))))))

(defgeneric group-withfixtures-setup-thunk (record))

(defgeneric group-withfixtures-cleanup-thunk (record))

(defgeneric group-eachtest-setup-thunk (record))

(defgeneric group-eachtest-cleanup-thunk (record))

(defgeneric do-test-fixture-assignment (test-obj)
  (:documentation
   "Fixture declarations translate to an :around method making let* bindings
for the test application class.")
  (:method (test-obj)
     (format-at-verbosity 4 "Called do-test-fixture-assignment principal method ~s~%"
       test-obj)
     (with-nst-control-handlers
         ((e flag
             :cerror-label-var exit-tests-label
             :cerror-label (format nil
                               "Continue with other tests in this group (~s)"
                             (group-name test-obj))
             :handler-return-to do-test-fixture-assignment)
          (format-at-verbosity 4
              "In the setup handler for do-test-fixture-assignment ~s~%"
            test-obj)
          (setf (gethash (check-group-name test-obj) +results-record+)
            (make-config-error e test-obj
                               "Error in test post-fixture setup")))
       (funcall (test-setup-form test-obj)))
     (unwind-protect (core-run-test test-obj)
       (with-nst-control-handlers
           ((e flag
               :cerror-label-var exit-tests-label
               :cerror-label (format nil
                                 "Continue with other tests in this group (~s)"
                               (group-name test-obj))
               :handler-return-to do-test-fixture-assignment)
            (format-at-verbosity 4
                "In the cleanup handler for do-test-fixture-assignment ~s~%"
              test-obj)
            (add-test-config-error test-obj
              "Error in test fixtures cleanup: ~s" e))
         (funcall (test-cleanup-form test-obj))))))

(defun wrap-fixture-name-specials (fixture-names-special form)
  (cond
   ((null fixture-names-special) form)
   ((equal fixture-names-special '(special)) form)
   (t `(locally (declare ,fixture-names-special) ,form))))

(defun core-run-test (test)
  "Capture the result of the test."
  (declare (optimize (debug 3)))
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
                                  (named-function core-run-test-warning-handler
                                    (lambda (w)
                                      (push w caught-warnings)
                                      (muffle-warning w)))))

                    (let ((*current-group* (group-name test))
                          (*current-test*  (test-name-lookup test))
                          (forms (test-forms test))
                          (fixture-names-special (special-fixture-names test))
                          (criterion (test-criterion test)))
                      (declare (special *current-group* *current-test*))

                      (with-retry ((format nil "Try running test ~s again"
                                     *nst-check-user-name*))
                        (cond
                         ((eql 1 (length forms))
                          (check-criterion-on-form
                           criterion
                           `(common-lisp:multiple-value-list
                                ,(wrap-fixture-name-specials
                                  fixture-names-special (car forms)))
;;;                             (locally (declare ,fixture-names-special)
;;;                               ,(car forms))
                           ))
                         (t (check-criterion-on-form
                             criterion
                             (wrap-fixture-name-specials fixture-names-special
                                                         `(list ,@forms))
;;;                             `(locally (declare ,fixture-names-special)
;;;                                (list ,@forms))
                             )))))))
          (end-time (get-internal-real-time)))
      (setf (result-stats-elapsed-time result) (- end-time start-time)
            (gethash (check-group-name test) +results-record+) result)
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
