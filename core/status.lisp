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

#+allegro
(defmacro make-backtrace-lines ()
  `(let ((raw (with-output-to-string (stream)
                (let ((top-level:*zoom-print-circle* nil)
                      (*print-right-margin* 1000000))
                  (declare (special top-level:*zoom-print-circle*
                                    *print-right-margin*))
                  (top-level.debug:zoom stream :function nil :verbose nil
                                        :moderate t :specials nil
                                        :length 5 :level nil)))))

     (let ((lines (loop for spot = (position #\Newline raw)
                      while spot
                      collect (string-left-trim " ->" (subseq raw 0 spot))
                      do (setf raw (subseq raw (+ 1 spot))))))
       (pop lines)
       (pop lines)

       (loop while (not (search "emit-error " (car lines))) do (pop lines))
       (pop lines)
       (if (search ":internal" (car lines)) (pop lines))
       (loop while (search "core-run-test" (car lines)) do (pop lines))
       (let ((first (position-if #'(lambda (x) (search "core-run-test" x))
                                 lines)))
         (setf lines (subseq lines 0 first)))

       lines)))

(defun emit-error (e &rest format-args &aux format args)
  (declare (special *nst-context* *nst-stack*))
  (cond
    (format-args (setf format (car format) args (cdr args)))
    (t (setf format "~w" args (list e))))
  (let ((other-args nil))
    ;; #+allegro (setf other-args (list* :zoom (make-backtrace-lines) other-args))
    (make-check-result :erring 1
                       :errors (list (apply #'make-error-check-note
                                            :context *nst-context*
                                            :stack *nst-stack*
                                            :format format
                                            :args args
                                            :error e
                                            other-args)))))

;;;
;;; Result records for high-level checks.
;;;

(defstruct (multi-results (:include result-stats))
  "Multiple results structure."
  package-reports group-reports test-reports system
  (stats-source nil))

(set-pprint-dispatch 'multi-results
  #'(lambda (s res)
      (with-accessors ((packages multi-results-package-reports)
                       (groups multi-results-group-reports)
                       (tests multi-results-test-reports)
                       (system multi-results-system)
                       (stats-source multi-results-stats-source)) res

        (when system
          (format s "~%Summary of results for system ~a:~%"
            (slot-value system 'asdf::name)))
        (let ((reports
               (nconc (loop for report in packages
                            collect (let ((*nst-report-driver* (case *nst-report-driver*
                                                                 (:details :details)
                                                                 (t :package))))
                                      (declare (special *nst-report-driver*))
                                      (format s "~w~%" report)
                                      report))
                      (loop for report in groups
                            collect (let ((*nst-report-driver* (case *nst-report-driver*
                                                                 (:details :details)
                                                                 (t :group))))
                                      (declare (special *nst-report-driver*))
                                      (format s "~w~%" report)
                                      report))
                      (loop for report in tests
                            collect (let ((*nst-report-driver* (case *nst-report-driver*
                                                                 (:details :details)
                                                                 (t :test))))
                                      (declare (special *nst-report-driver*))
                                      (format s "~w~%" report)
                                      report)))))
          (multiple-value-bind (code total passed erred failed warned)
              (result-summary (cond
                                (stats-source stats-source)
                                (t reports)))
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
                 (loop for check being the hash-keys of checks collect check))
                (*nst-group-shown* t))
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

(defun interesting-result-p (result)
  (when (typep result 'check-result)
    (with-accessors ((warnings check-result-warnings)
                     (failures check-result-failures)
                     (errors check-result-errors)
                     ;; (info check-result-info)
                     ) result
      (or warnings failures errors))))

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



(set-pprint-dispatch 'check-result
  #'(lambda (s cr)
      (declare (special *nst-verbosity* *nst-report-driver*))
      (with-accessors ((check-name check-result-check-name)
                       (group-name check-result-group-name)
                       (warnings check-result-warnings)
                       (failures check-result-failures)
                       (errors check-result-errors)
                       (info check-result-info)) cr
        (let ((total-items (+ (length warnings) (length failures)
                              (length errors)))
              (succeeded (eql 0 (+ (length failures) (length errors))))
              (drill-down (or (eq *nst-report-driver* :details)
                              (eq *nst-report-driver* :details)
                              (> *nst-verbosity* 2)
                              (and (eq *nst-report-driver* :test)
                                   (> *nst-verbosity* 1)))))
;;;          (format t
;;;               "drill-down ~s~%*nst-verbosity* ~s~%*nst-report-driver* ~s~%"
;;;             drill-down *nst-verbosity* *nst-report-driver*)

          (cond
           ;; The first three cases are for when we have only one
           ;; item to report.  We do this on one line if it fits, and
           ;; don't bother with bullet points.
           ;;
           ((and (eql 1 total-items) errors)
            (format s "~@<Check ~a ~:[(group ~a) ~;~*~]raised an error~
                            ~:[~2*~;:~{~:@_ . ~w~}~{~:@_ . ~a~}~]~:>"
              check-name *nst-group-shown* group-name
              drill-down errors info))

           ((and (eql 1 total-items) warnings)
            (format s "~@<Check ~a ~:[(group ~a) ~;~*~]passed with warning~p~
                            ~:[~2*~;:~{~:@_ - ~w~}~{~:@_ - ~a~}~]~:>"
              check-name *nst-group-shown* group-name
              warnings drill-down warnings info))

           ((and (eql 1 total-items) failures)
            (format s "~@<Check ~a ~:[(group ~a) ~;~*~]failed~
                            ~:[~2*~;:~{~:@_ - ~w~}~{~:@_ - ~a~}~]~:>"
              check-name *nst-group-shown* group-name
              drill-down failures info))

           ;; When a query asks about a specific test.
           ;;
           ((eq *nst-report-driver* :test)
            (format s "~@<Check ~a ~:[(group ~a) ~;~*~]~:[failed~;passed~]~
                         ~:[~;: ~@{~:[~2*~;~:@_~a~{~:@_ - ~w~}~]~}~]~:>"
              check-name *nst-group-shown* group-name succeeded
              (or errors failures warnings info)
              errors "Errors:" errors
              failures "Failures:" failures
              warnings "Warnings:" warnings
              info "Additional information:" info))

           ;; If we're reporting results for a package or group,
           ;; suppress the info fields of the report.
           ;;
           (t (format s "Check ~a ~:[(group ~a) ~;~*~]~
                                 ~:[failed~*~;passed~:[~; with warnings~]~]~
                           ~:[~;: ~@<~@{~:[~2*~;~a~:@_~{ - ~w~^~:@_~}~]~}~:>~]"
                check-name *nst-group-shown* group-name succeeded warnings
                (or errors failures warnings)
                errors "Errors:" errors  failures "Failures:" failures
                warnings "Warnings:" warnings)))))))




(defstruct context-layer
  "A record of test criterion
 criterion - the criterion symbol itself
 criterion-args - arguments to the criterion
 given-stack - the stack of values assessed by the criterion"
  criterion criterion-args given-stack)

(set-pprint-dispatch 'context-layer
  #'(lambda (s cl)
      (with-accessors ((criterion context-layer-criterion)
                       (criterion-args context-layer-criterion-args)
                       (given-stack context-layer-given-stack)) cl
        (format s "checki~@<ng (~s~@<~{~:_ ~s~}~:>) ~_on (~{~a~^ ~})~:>"
          criterion criterion-args given-stack))))


(defstruct check-note
  "A single note issued in criteria checking.
 context - the surrounding criteria structure, a list of context-layer structs
 stack - the stack of values at the note point
 format, args - further details; this string and list may e.g. be provided to
                cl:format"
  context stack format args)

(set-pprint-dispatch 'check-note
  #'(lambda (s cn)
      (with-accessors ((context check-note-context)
                       (stack check-note-stack)
                       (format check-note-format)
                       (args check-note-args)) cn
        (declare (ignorable context stack))
        (format s "~@<~:[~2*~;~?~:@_~]~
                      in context: ~@<~{~a~^~:@_~}~:>~
                      ~@[~:@_stack: ~w~]~:>"
          format format args
          context
          stack)
        )))


(defstruct (error-check-note (:include check-note))
  "A note issued in regards to a thrown error."
  error #+allegro zoom)

(set-pprint-dispatch 'error-check-note
  #'(lambda (s cn)
      (with-accessors ((context check-note-context)
                       (stack check-note-stack)
                       (format check-note-format)
                       (args check-note-args)
                       (error error-check-note-args)) cn
        (declare (ignorable context stack))
        (let (#+allegro (show-zoom (or (eq *nst-report-driver* :test)
                                       (eq *nst-report-driver* :details)
                                       (> *nst-verbosity* 2))))
          (format s "~@<~w~:[~2*~;~:@_~?~]~
                        ~:@_~:[nil context~;~:*in context: ~@<~{~a~^~:@_~}~:>~]~
                        ~:@_~:[nil values~;~:*values: ~w~]~
                        ~:[~*~;~@[~:@_at ~@<~{~a~^ ~:@_while ~}~:>~]~]~:>"
            error format format args context stack
            #-allegro nil #-allegro nil
            #+allegro show-zoom #+allegro (error-check-note-zoom cn))))))

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

  (:method ((r multi-results) &optional
            (code :clear) (total 0) (passed 0) (erred 0) (failed 0)
            (warned 0) (other nil))
      (with-accessors ((packages multi-results-package-reports)
                       (groups multi-results-group-reports)
                       (tests multi-results-test-reports)
                       (stats-source multi-results-stats-source)) r
        (cond
          (stats-source
           (result-summary stats-source
                           code total passed erred failed warned other))
          (t
           (result-summary packages
                           code total passed erred failed warned
                           (list* groups tests other))))))

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
    (when (> *nst-verbosity* 3)
      (format t "Reporting for actual package ~s~%" user-package)
      (format t "sym-pack ~s~%" sym-pack))
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

(defun use-stats-from (stats-source stats-dest)
  (setf (result-stats-elapsed-time stats-dest)
        (result-stats-elapsed-time stats-source)
        (result-stats-tests stats-dest)   (result-stats-tests stats-source)
        (result-stats-passing stats-dest) (result-stats-passing stats-source)
        (result-stats-erring stats-dest)  (result-stats-erring stats-source)
        (result-stats-failing stats-dest) (result-stats-failing stats-source)
        (result-stats-warning stats-dest) (result-stats-warning stats-source))
  stats-dest)

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

(defun report-interesting ()
  (let ((test-reports (loop for test-report being the hash-values
                            of +results-record+
                            if (interesting-result-p test-report)
                              collect test-report)))

    (make-multi-results :package-reports nil
                        :group-reports nil
                        :test-reports test-reports
                        :system nil
                        :stats-source (all-package-report))))

;;;
;;; Printing functions
;;;

(defun report-package (&optional
                       (package *package*)
                       (stream *nst-output-stream*)
                       (*nst-verbosity* (max *default-report-verbosity*
                                             *nst-verbosity*)))
  "Top-level function for reporting the results of the tests in a package."
  (let ((*nst-report-driver* :package)
        (*print-pretty* t)
        (*print-readably* nil))
    (declare (special *nst-report-driver*))
    (format stream "~w" (package-report package))
    nil))

(defun report-group (group
                     &optional
                     (stream *nst-output-stream*)
                     (*nst-verbosity* (max *default-report-verbosity*
                                           *nst-verbosity*)))
  "Top-level function for reporting the results of the tests in a group."
  (let ((*nst-report-driver* :group)
        (*print-pretty* t)
        (*print-readably* nil))
    (declare (special *nst-report-driver*))
    (format stream "~w" (group-report group))
    nil))

(defun report-test (group
                    test &optional
                    (stream *nst-output-stream*)
                    (*nst-verbosity* (max *default-report-verbosity*
                                          *nst-verbosity*)))
  "Top-level function for reporting the results of a test."
  (let ((*nst-report-driver* :test)
        (*print-pretty* t)
        (*print-readably* nil))
    (declare (special *nst-report-driver*))
    (format stream "~w" (test-report group test))
    nil))

(defun report-multiple (packages groups tests &key
                                 (stream *nst-output-stream*)
                                 (verbosity *default-report-verbosity*)
                                 (system nil system-supp-p))
  "Top-level function for reporting the results of several tests."
  (let ((*nst-report-driver* :multiple)
        (*nst-verbosity* (max verbosity *nst-verbosity*))
        (*print-pretty* t)
        (*print-readably* nil)
        (report (apply #'multiple-report
                       packages groups tests
                       (cond
                         (system-supp-p `(:system ,system))
                         (t nil)))))
    (declare (special *nst-verbosity* *nst-report-driver*))
    (format stream "~w" report)
    nil))





(defun report-summary (group-or-package gp-supp-p test test-supp-p)
  (cond
   ((not gp-supp-p)
    (let ((*print-pretty* t)
          (*print-readably* nil)
          (*nst-verbosity* 1)
          (*nst-report-driver* :multiple))
      (pprint (report-interesting) *nst-output-stream*)))

   (test-supp-p (report-test group-or-package test))

   ((find-package group-or-package) (report-package group-or-package))

   (t (report-group group-or-package)))
  nil)

(defun report-details (group-or-package gp-supp-p test test-supp-p)
  (let ((report (cond
                  ((not gp-supp-p) (report-interesting))
                  (test-supp-p (test-report group-or-package test))
                  ((find-package group-or-package)
                   (package-report group-or-package))
                  (t (group-report group-or-package))))
        (*print-pretty* t)
        (*print-readably* nil)
        (*nst-verbosity* 2)
        (*nst-report-driver* :details))
    (pprint report *nst-output-stream*)
    nil))



(defun nst-dump (&key (stream *nst-output-stream*)
                      (verbosity *default-report-verbosity*))
  "Spit out the full NST state."
  (let ((report (all-package-report))
        (*print-pretty* t) (*print-readably* nil)
        (*nst-verbosity* verbosity))
    (declare (special *nst-verbosity*))
    (format stream "NST globals:~%")
    (format stream " - *nst-verbosity*: ~s (~s)~%"
      (nst-repl-property-display :verbose) *nst-verbosity*)
    (format stream " - *default-report-verbosity*: ~s~%"
      *default-report-verbosity*)
    (format stream " - *nst-output-stream*: ~s~%" *nst-output-stream*)
    (format stream " - *debug-on-error*: ~s~%" *debug-on-error*)
    (format stream " - *nst-info-shows-expected*: ~s~%"
      *nst-info-shows-expected*)
    (format stream "Stored test results:~%")
    (format stream "~w" report)))

;;;
;;; Generating status data within checks.
;;;

(defun emit-warning (&key format args)
  "For use within user-defined check criteria: emit a warning."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (check-result
   :warnings (list (make-check-note :context *nst-context* :stack *nst-stack*
                                    :format format :args args))))

(defun emit-failure (&key format args info)
  "For use within user-defined check criteria: explain a failure."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (check-result
   :failures (list (make-check-note :context *nst-context* :stack *nst-stack*
                                    :format format :args args))
   :info info))
(defun add-failure (result &key format args)
  "For use within user-defined check criteria: add a failure to a result."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))

  (push (make-check-note :context *nst-context* :stack *nst-stack*
                         :format format :args args)
        (check-result-failures result)))

(defun emit-success ()
  "For use within user-defined check criteria: record a successful check."
  (check-result))

(defun add-error (result &key format args)
  "For use within user-defined check criteria: add an error to a result."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))

  (push (make-check-note :context *nst-context* :stack *nst-stack*
                         :format format :args args)
        (check-result-errors result)))

(defun add-info (result item)
  "For use within user-defined check criteria: add an info note to a result."
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (push item (check-result-info result)))
