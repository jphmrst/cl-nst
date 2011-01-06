;;; File status.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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
  `(block backtrace-maker
     (let ((raw (with-output-to-string (stream)
                  (let ((top-level:*zoom-print-circle* nil)
                        (*print-right-margin* 1000000))
                    (declare (special top-level:*zoom-print-circle*
                                      *print-right-margin*))
                    (top-level.debug:zoom stream :function nil :verbose nil
                                          :moderate t :specials nil
                                          :length 5 :level nil)))))

       (handler-bind
           ((error
             #'(lambda (cnd)
                 (declare (ignore cnd))
                 (format-at-verbosity 3 "Caught error formatting backtrace~%")
                 (return-from backtrace-maker
                   (list "Caught error while formatting backtrace, returning raw lines"
                         raw)))))
         (let ((lines (loop for spot = (position #\Newline raw)
                          while spot
                          collect (string-left-trim " ->" (subseq raw 0 spot))
                          do (setf raw (subseq raw (+ 1 spot))))))
           (unless (search ,(if (string= "zz" (symbol-name 'zz))
                              "make-error-report "
                              "MAKE-ERROR-REPORT ") (car lines))  (pop lines))
           (unless (search ,(if (string= "zz" (symbol-name 'zz))
                              "make-error-report "
                              "MAKE-ERROR-REPORT ") (car lines))  (pop lines))

           (let ((orig-lines (loop for line in lines collect line)))

             (handler-bind
                 ((error #'(lambda (cnd)
                             (declare (ignore cnd))
                             (format-at-verbosity 3
                                 "Caught error ~s identifying backtrace core~%"
                               cnd)
                             (return-from backtrace-maker
                               (list* "Caught error while identifying backtrace core, returning raw lines"
                                      lines)))))
               (loop while (and lines
                                (not (search ,(if (string= "zz"
                                                           (symbol-name 'zz))
                                                "make-error-report "
                                                "MAKE-ERROR-REPORT ")
                                             (car lines))))
                 do (pop lines))
               (cond
                ;; We found the "make-error-report" line, and it's not at the
                ;; top of the list of lines.
                (lines (pop lines))

                ;; There is no "make-error-report" line, so restore the
                ;; original list of lines.
                (t (setf lines orig-lines)))
               (if (search ,(if (string= "zz" (symbol-name 'zz))
                              ":internal"
                              ":INTERNAL") (car lines)) (pop lines))
               (loop while (search ,(if (string= "zz" (symbol-name 'zz))
                                      "core-run-test"
                                      "CORE-RUN-TEST")
                                   (car lines))
                   do (pop lines))
               (let ((first
                      (position-if #'(lambda (x)
                                       (search ,(if (string= "zz"
                                                             (symbol-name 'zz))
                                                    "core-run-test"
                                                    "CORE-RUN-TEST") x))
                                         lines)))
                 (setf lines (subseq lines 0 first)))
               lines)))))))

(defmacro emit-error (&rest args)
  "Deprecated; use make-success-report."
  (warn 'nst-soft-deprecation :old-name 'emit-error
        :replacement '(make-error-report))
  `(make-error-report ,@args))

(defun make-error-report (e &rest format-args &aux format args)
  (declare (special *nst-context* *nst-stack*))
  (cond
   (format-args (setf format (car format) args (cdr args)))
   (t (setf format "~w" args (list e))))
  (let ((other-args nil))
    #+(and allegro (not macosx))
    (setf other-args (list* :zoom (make-backtrace-lines) other-args))
    (let ((error-note (apply #'make-error-check-note
                             :context *nst-context*
                             :stack *nst-stack*
                             :format format
                             :args args
                             :error e
                             other-args)))
      (make-and-calibrate-check-result :errors (list error-note)))))
(def-documentation (function make-error-report)
  (:tags criteria)
  (:properties (api-summary criteria))
  (:intro (:latex "Function \\texttt{make-error-report} produces a report of an error during test execution."))
  (:callspec (&key (format format-string) (args arg-form-list))))

(defun make-config-error (error test-obj msg)
  (let ((*nst-group-name* (group-name test-obj))
        (*nst-check-user-name* (test-name-lookup test-obj)))
    (declare (special *nst-group-name* *nst-check-user-name*))
    (make-error-report error :format (format nil msg))))


(defun fixture-binding-error-note (fixture-name variable-name error)
  (make-error-report
   error
   (format nil "~~@<Error binding ~a for fixture ~s:~~_ ~~w~~:>"
     variable-name fixture-name)
   error))

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
          (when (group-record-p name)
            (setf name (group-name name)))
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


(defstruct (check-result (:include result-stats (tests 1))
                         (:constructor %make-check-result))
  "Overall check result structure, containing notes of four distinct types.  A
note is an instance of the check-note structure below.  The four note types are:
 warnings - generated warnings
 failures - criteria which are not met, but not a Lisp error
 errors - Lisp errors
 info - supplimentary information
Each of these fields is a list; warnings, failures and errors are check-note
instances, and the info field is of any value."
  (group-name *nst-group-name*)
  (check-name *nst-check-user-name*)
  (warnings nil) (failures nil) (errors nil) (info nil))

(defun make-check-result (&key (group-name *nst-group-name*)
                               (check-name *nst-check-user-name*)
                               warnings failures errors info
                               (tests 0) (passing 0) (erring 0)
                               (failing 0) (warning 0)
                               (elapsed-time 0)
                               (timestamp (multiple-value-list
                                              (get-decoded-time))))
  (%make-check-result :group-name group-name
                      :check-name check-name
                      :warnings warnings
                      :failures failures
                      :errors errors
                      :info info
                      :tests tests :passing passing :erring erring
                      :failing failing :warning warning
                      :elapsed-time elapsed-time
                      :timestamp timestamp))
(def-documentation (function make-check-result)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro "Functional wrapper around the constructor for check-result
structure, permitting the use of apply."))

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

(defun make-and-calibrate-check-result (&rest args)
  (calibrate-check-result (apply #'make-check-result args)))

(defmacro check-result (&rest args)
  (warn 'nst-soft-deprecation :old-name 'check-result
        :replacement '(make-success-report emit-failure emit-warning))
  `(make-and-calibrate-check-result ,@args))

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
                              (> *nst-verbosity* 2)
                              (and (eq *nst-report-driver* :test)
                                   (> *nst-verbosity* 1)))))

          (flet ((headered-items-printer (stream header items &key numbered)
                   (when items
                     (format stream "~:@_  ~a" header)
                     (loop for item in items for num from 1 do
                       (pprint-newline :mandatory stream)
                       (cond
                         ((and numbered (> (length items) 1))
                          (format stream "   ~d. " num))
                         (t (princ "   - " stream)))
                       (princ item stream))))

                 (unheadered-items-printer (stream items &key numbered)
                   (loop for item in items for num from 1 do
                     (pprint-newline :mandatory stream)
                       (cond
                         ((and numbered (> (length items) 1))
                          (format stream " ~d. " num))
                         (t (princ " - " stream)))
                     (princ item stream))))

;;;            (format t
;;;                "drill-down ~s~%*nst-verbosity* ~s~%*nst-report-driver* ~s~%~
;;;                 info ~s~%"
;;;              drill-down *nst-verbosity* *nst-report-driver* info)

            (pprint-logical-block (s '(dummy list))
              (format s "Check ~a " check-name)
              (unless *nst-group-shown*
                (format s "(group ~a) " group-name))

              (cond
               ;; The first three cases are for when we have only one
               ;; item to report.  We do this on one line if it fits, and
               ;; don't bother with bullet points.
               ;;
               ((and (eql 1 total-items) errors)
                (princ "raised an error" s)
                (when drill-down
                  (princ ":" s)
                  (unheadered-items-printer s errors :numbered t)
                  (headered-items-printer s "Additional information:" info)))

               ((and (eql 1 total-items) warnings)
                (cond
                  ((eql 1 (length warnings))
                   (format s "passed with a warning"))
                  (t (format s "passed with warnings")))
                (unheadered-items-printer s warnings :numbered t)
                (headered-items-printer s "Additional information:" info))

               ((and (eql 1 total-items) failures)
                (princ "failed" s)
                (unheadered-items-printer s failures :numbered t)
                (headered-items-printer s "Additional information:" info))

               ;; When a query asks about a specific test.
               ;;
               ((eq *nst-report-driver* :test)
                (cond
                 (succeeded (princ "passed" s))
                 (t (princ "failed" s)))
                (when (or errors failures warnings info)
                  (princ ": " s)
                  (headered-items-printer s "Errors:" errors :numbered t)
                  (headered-items-printer s "Failures:" failures :numbered t)
                  (headered-items-printer s "Warnings:" warnings :numbered t)
                  (headered-items-printer s "Additional information:" info)))

               ;; If we're reporting results for a package or group,
               ;; suppress the info fields of the report.
               ;;
               (t
                (cond (succeeded
                       (princ "passed" s)
                       (when warnings
                         (princ " with warning" s)
                         (when (> (length warnings) 1)
                           (princ "s" s))))
                      (errors (princ "raised an error" s))
                      (t (princ "failed" s)))
                (cond
                  ((and (not errors) (not failures) warnings)
                   (unheadered-items-printer s warnings :numbered t)
                   (headered-items-printer s "Additional information:"
                                           info))
                 ((or errors failures)
                  (princ ": " s)
                  (headered-items-printer s "Errors:" errors :numbered t)
                  (headered-items-printer s "Failures:" failures :numbered t)
                  (headered-items-printer s "Further warnings:" warnings
                                          :numbered t)
                  (headered-items-printer s "Additional information:"
                                          info))
                 (t (princ "." s)))))))))))


(defstruct check-note
  "A single note issued in criteria checking.
 context - the surrounding criteria structure, a list of context-layer structs
 stack - the stack of values at the note point
 format, args - further details; this string and list may e.g. be provided to
                cl:format"
  context stack format args)

(defun apply-formatter (stream formatter args)
  (cond
    ((null formatter) nil)
    ((functionp formatter) (apply formatter stream args) t)
    ((stringp formatter)   (apply #'format stream formatter args) t)
    (t (error "Cannot handle formatter ~s" formatter))))

(set-pprint-dispatch 'check-note
  #'(lambda (s cn)
      (with-accessors ((context check-note-context)
                       (stack check-note-stack)
                       (format check-note-format)
                       (args check-note-args)) cn
        (pprint-logical-block (s '(dummy list))
          (apply-formatter s format args)
          (when (or (> *nst-verbosity* 2)
                    (and (> *nst-verbosity* 1)
                         (eq *nst-report-driver* :details)))
            (format s "~{~:@_~w~}" (get-display-context-layers context))
            (when stack
              (pprint-newline :mandatory s)
              (princ "stack: " s)
              (princ stack s)))))))

(defstruct (error-check-note (:include check-note))
  "A note issued in regards to a thrown error."
  error #+allegro zoom)

(set-pprint-dispatch 'error-check-note
  #'(lambda (s cn)
      (with-accessors ((context check-note-context)
                       (stack check-note-stack)
                       (format check-note-format)
                       (args check-note-args)
                       (error error-check-note-error)) cn
        (let (#+allegro (show-zoom (or (eq *nst-report-driver* :test)
                                       (eq *nst-report-driver* :details)
                                       (> *nst-verbosity* 2))))
          (pprint-logical-block (s '(dummy list))
            (format s "~a" error)
            (when (apply-formatter s format args)
              (pprint-newline :mandatory s))
            (cond
              (context (format s "~:@_in context: ~@<~{~a~^~:@_~}~:>" context))
              (t (princ "at top level" s)))
            (pprint-newline :mandatory s)
            (format s "~:[nil values~;~:*values: ~w~]" stack)
            #+allegro (when show-zoom
                        (pprint-newline :mandatory s)
                        (princ "at " s)
                        (pprint-logical-block (s (error-check-note-zoom cn))
                          (loop do
                            (write (pprint-pop) :stream s
                                   :escape nil :readably nil :pretty t
                                   :circle nil)
                            (pprint-exit-if-list-exhausted)
                            (pprint-newline :mandatory s)
                            (princ "while " s)))))))))

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
  (let ((result (make-package-result))
        (user-package (find-package package)))
    (unless user-package
      (error "No such package ~s" package))
    (let ((sym-pack (loop for k being the hash-keys
                        of (gethash user-package +package-groups+)
                        collect k)))
      (format-at-verbosity 3 "Reporting for actual package ~s~%sym-pack ~s~%"
        user-package sym-pack)
      (when sym-pack
        (with-accessors ((name package-result-package-name)
                         (checks package-result-group-results)) result
          (setf name (package-name user-package))
          (loop for remote-group in sym-pack do
            (let* ((local-group (intern (symbol-name remote-group)
                                        user-package))
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
      result)))

(defun group-report (group)
  "Top-level function for reporting the results of a group."
  (let ((result (make-group-result)))
    (with-accessors ((the-check-results group-result-check-results)
                     (the-group-name group-result-group-name)
                     (total-elapsed-time result-stats-elapsed-time)
                     (total-tests        result-stats-tests)
                     (total-passing      result-stats-passing)
                     (total-erring       result-stats-erring)
                     (total-failing      result-stats-failing)
                     (total-warning      result-stats-warning)) result
      (setf the-group-name group)
      (loop for test in (test-names group)
            for report = (test-report group test)
            do
         (setf (gethash test the-check-results) report)
         (cond
           (report
            (incf total-elapsed-time (result-stats-elapsed-time report))
            (incf total-tests        (result-stats-tests report))
            (incf total-passing      (result-stats-passing report))
            (incf total-erring       (result-stats-erring report))
            (incf total-failing      (result-stats-failing report))
            (incf total-warning      (result-stats-warning report)))
           (t (incf total-tests)))))
    result))

(defun test-report (group test)
  "Top-level function for reporting the results of a test."
  (let ((test-inst (ensure-test-instance group test)))
    (gethash (check-group-name test-inst) +results-record+)))

(defun multiple-report (packages groups tests &key system)
  (let ((group-source (copy-seq groups)))
    (setf groups nil)
    (loop while group-source do
      (let ((g (pop group-source)))
        (push g groups)
        (loop for add in (group-include-groups g) do
          (push (make-instance add) group-source)))))
  (let* ((package-reports (loop for p in packages collect (package-report p)))
         (group-reports (loop for g in groups collect (group-report g)))
         (test-reports
          (loop for item in tests
              append (cond
                      ((consp item)
                       (destructuring-bind (g . ts) item
                         (list (test-report g ts))))

                      ((test-record-p item)
                       (list (test-report (group-name item)
                                          (test-name-lookup item))))

                      (t nil))))
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
  (when result
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
      result)))

(defun all-package-report ()
  (let ((package-hash (make-hash-table :test 'eq)))
    (loop for test-report being the hash-values of +results-record+ do
      (when test-report
        (setf (gethash (symbol-package (check-result-group-name test-report))
                       package-hash)
              t)))
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





(defun get-report-from-names (group-or-package gp-supp-p test test-supp-p)
  (cond
   ((not gp-supp-p) (report-interesting))
   (test-supp-p (test-report group-or-package test))
   ((find-package group-or-package) (package-report group-or-package))
   ((and (find-class group-or-package nil)
         )
    (group-report group-or-package))
   (t (let ((interps (lookup-artifact group-or-package)))
         (cond
          ((null interps)
           (format t "There is no NST-testable unit with the name ~a.~%"
             group-or-package))

          (t
           (let ((packages nil) (groups nil) (tests nil))
             (loop for interp in interps do
               (cond
                 ((packagep interp)
                  (push interp packages))

                ((group-record-p interp)
                  (push interp groups))

                (t
                 (push interp tests))))
             (multiple-report packages groups tests))))))))

(defun report-summary (group-or-package gp-supp-p test test-supp-p)
  (let ((report (get-report-from-names group-or-package gp-supp-p
                                       test test-supp-p))
        (*print-pretty* t)
        (*print-readably* nil)
        (*nst-verbosity* 1)
        (*nst-report-driver* :multiple))
    (pprint report *nst-output-stream*))
  nil)

(defun report-details (group-or-package gp-supp-p test test-supp-p)
  (let ((report (get-report-from-names group-or-package gp-supp-p
                                       test test-supp-p))
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
    (format stream " - *debug-on-fail*: ~s~%" *debug-on-fail*)
    (format stream " - *nst-info-shows-expected*: ~s~%"
      *nst-info-shows-expected*)
    (format stream "Stored test results:~%")
    (format stream "  ~w" report)))

;;;
;;; Generating status data within checks.
;;;

(defun make-warning-report (&key format args)
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (make-success-report :warnings (list (make-check-note :context *nst-context*
                                                        :stack *nst-stack*
                                                        :format format
                                                        :args args))))
(def-documentation (function make-warning-report)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro (:latex "Function \\texttt{make-warning-report} is like \\texttt{make-failure-report}, but provides supplimentary information as a warning."))
  (:callspec (&key (format format-string) (args arg-form-list)))
  (:details (:latex "The \\texttt{emit-warning} function is an older, deprecated version of this function.")))

(defmacro emit-warning (&rest args)
  (warn 'nst-soft-deprecation :old-name 'emit-warning
        :replacement '(make-warning-report))
  `(make-warning-report ,@args))
(def-documentation (compiler-macro emit-warning)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb (:latex "The \\texttt{emit-warning} function is deprecated; use \\texttt{make-warning-report} instead.")))

(defun make-failure-report (&key format args info)
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (make-and-calibrate-check-result
   :failures (list (make-check-note :context *nst-context*
                                    :stack *nst-stack*
                                    :format format :args args))
   :info info))
(def-documentation (function make-failure-report)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro (:latex "The \\texttt{make-failure-report} function returns a report of test failure."))
  (:callspec (&key (format format-string) (args arg-form-list)))
  (:details (:latex "The \\texttt{format-string} and \\texttt{args} are as to the Common Lisp function \\texttt{format}.  The \\texttt{emit-failure} function is an older, deprecated version of this function.")))

(defmacro emit-failure (&rest args)
  (warn 'nst-soft-deprecation :old-name 'emit-failure
        :replacement '(make-failure-report))
  `(make-failure-report ,@args))
(def-documentation (compiler-macro emit-failure)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb (:latex "The \\texttt{emit-failure} function is deprecated; use \\texttt{make-failure-report} instead.")))

(defun make-success-report (&rest args &key warnings info)
  (declare (ignore warnings info))
  (apply #'make-and-calibrate-check-result args))
(def-documentation (function make-success-report)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro (:latex "The \\texttt{make-success-report} function indicates a successful test result."))
  (:callspec ())
  (:details (:latex "Note that some older examples show \\texttt{(make-check-result)}, \\texttt{(emit-success)} or \\texttt{(check-result)}.  The former is an internal function and should not be used from outside the core NST files.  The latter two are deprecated.")))

(defmacro emit-success (&rest args)
  (warn 'nst-soft-deprecation :old-name 'emit-success
        :replacement '(make-success-report))
  `(make-success-report ,@args))
(def-documentation (compiler-macro emit-success)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb (:latex "The \\texttt{emit-success} function is deprecated; use \\texttt{make-success-report} instead.")))

(defun add-failure (result &key format args)
  (declare (special *nst-context* *nst-stack* *nst-check-name*))

  (push (make-check-note :context *nst-context* :stack *nst-stack*
                         :format format :args args)
        (check-result-failures result)))
(def-documentation (function add-failure)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:blurb (:latex
             "For use within user-defined NST criteria: add a failure to a result."))
    (:intro (:latex
             "The \\texttt{add-failure} function adds a failure note to a result record."))
    (:callspec (result-report &key
                              (format format-string) (args argument-list))))

(defun add-error (result &key format args)
  (declare (special *nst-context* *nst-stack* *nst-check-name*))

  (push (make-check-note :context *nst-context* :stack *nst-stack*
                         :format format :args args)
        (check-result-errors result)))
(def-documentation (function add-error)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:blurb (:latex "For use within user-defined NST criteria: add an error to a result."))
    (:intro (:latex
             "The \\texttt{add-error} function adds an error note to a result record."))
    (:callspec (result-report &key
                              (format format-string) (args argument-list))))

(defun add-test-config-error (test-obj format &rest args)
  (let ((*nst-group-name* (group-name test-obj))
        (*nst-check-user-name* (test-name-lookup test-obj))
        (report (gethash (check-group-name test-obj) +results-record+)))
    (declare (special *nst-group-name* *nst-check-user-name*))
    (add-error report :format format :args args)))

(defun add-info (result item)
  (declare (special *nst-context* *nst-stack* *nst-check-name*))
  (push item (check-result-info result)))
(def-documentation (function add-info)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:blurb "For use within user-defined NST criteria: add an info note to a result.")
    (:intro (:latex
             "The \\texttt{add-info} function adds auxiliary information to a result record."))
    (:callspec (result-report info-item)))

(defgeneric format-for-warning (stream item colon at-sign &rest params)
  (:documentation "Hook allowing us to sometimes do better than the usual
pretty-printer for warnings.")
  (:method (stream item colon at-sign &rest params)
           (declare (ignorable colon at-sign params))
           (format stream "~a" item))
  #+allegro
  (:method (stream (item simple-warning) colon at-sign &rest params)
     (declare (ignorable colon at-sign params))
     (cond
      ((slot-boundp item 'excl::format-control)
       (apply #'format stream (slot-value item 'excl::format-control)
              (slot-value item 'excl::format-arguments)))
      (t (call-next-method)))))
