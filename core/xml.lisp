;;; CURRENTLY EXCLUDED

;;; File xml.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
;;; Written by John Maraist.
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

(defvar *default-xml-pprint-dispatch* (copy-pprint-dispatch))

;;; -----------------------------------------------------------------
;;; General XML stuff.

(defun pprint-cdata-string (stream string)
  (format stream "<![CDATA[~a]]>" string))

(defmacro with-pprint-cdata ((stream-name) &body forms)
  `(progn
     (format ,stream-name "<![CDATA[")
     ,@forms
     (format ,stream-name "]]>")))

(defmacro with-xml-tagged-pprint-logical-block ((stream tag &key properties)
                                                &body forms)
  (when (eq stream t)
    (setf stream '*standard-output*))
  (let ((tag-val (gensym)))
    `(let ((,tag-val ,tag))
       (format ,stream "<~a" ,tag-val)
       ,@(when properties
           `((princ " " ,stream)
             (pprint-logical-block (,stream (list ,@(loop for p in properties
                                                        collect `(list ,@p))))
               (loop for (prop-name prop-val) = (pprint-pop) while prop-name do
                 (format ,stream "~a=\"~a\""
                         prop-name (string-escaped prop-val))
                 (pprint-exit-if-list-exhausted)
                 (princ " " ,stream)
                 (pprint-newline :fill ,stream)))))
       (princ ">" ,stream)
       (pprint-newline :mandatory ,stream)
       (princ "  " ,stream)
       (pprint-logical-block (,stream '(1 2))
         ,@forms)
       (pprint-newline :mandatory ,stream)
       (format ,stream "</~a>" ,tag-val))))

(defun symbol-to-junit-name (symbol)
  (let ((symbol (if (symbolp symbol)
                  symbol
                  (group-record-name symbol))))
    (format nil "lisp.~a.~a"
            (package-name (symbol-package symbol)) (symbol-name symbol))))

(defun junit-header (stream)
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>~%"))

;;; -----------------------------------------------------------------
;;; XUnit timestamps.

(defun elapsed-time-to-string (elapsed-time)
  (format nil "~f" (/ elapsed-time internal-time-units-per-second)))

(defun timestamp-to-string (timestamp)
  (destructuring-bind (second minute hour date month year day daylight-p zone)
      timestamp
    (declare (ignorable day daylight-p zone))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

;;; -----------------------------------------------------------------
;;; Printing result structures as XML.

(defun pprint-xml (object &optional (stream *standard-output*))
  (let ((*print-pprint-dispatch* *default-xml-pprint-dispatch*)
        #+allegro (tpl:*zoom-display* t)
        #+allegro (tpl:*zoom-print-circle* nil)
        #+allegro (tpl:*zoom-print-level*  nil)
        #+allegro (tpl:*zoom-print-length* nil))
    #+allegro (declare (special tpl:*zoom-print-level* tpl:*zoom-print-circle*
                                tpl:*zoom-display* tpl:*zoom-print-length*))
    (write object
           :stream stream :array t :circle nil :escape nil :level nil
           :length nil :lines nil :miser-width nil :pretty t :readably nil)))

(set-pprint-dispatch 'multi-results
  (named-function pprint-xml-multi-results
    (lambda (s item)
      (with-accessors ((elapsed-time result-stats-elapsed-time)
                       (tests result-stats-tests)
                       (errors result-stats-erring)
                       (failures result-stats-failing)
                       (system multi-results-system)
                       (group-reports multi-results-group-reports)) item
        (let ((actual-reports (loop for report in group-reports
                                  if report collect report)))
          (pprint-logical-block (s actual-reports)
            (loop for report = (pprint-pop) while report do
                  (write report :stream s)
                  (pprint-exit-if-list-exhausted)
                  (pprint-newline :mandatory s)))))))
  0 *default-xml-pprint-dispatch*)

(set-pprint-dispatch 'group-result
  (named-function pprint-xml-group-result
    (lambda (s item)
      (with-accessors ((elapsed-time result-stats-elapsed-time)
                       (tests result-stats-tests)
                       (errors result-stats-erring)
                       (failures result-stats-failing)
                       (system multi-results-system)
                       (group-name group-result-group-name)
                       (test-reports group-result-check-results)
                       (timestamp result-stats-timestamp)) item
        (with-xml-tagged-pprint-logical-block
            (s "testsuite" :properties
               (("errors"    (format nil "~d" errors))
                ("failures"  (format nil "~d" failures))
                ("name"      (symbol-to-junit-name group-name))
                ("tests"     (format nil "~d" tests))
                ("time"      (elapsed-time-to-string elapsed-time))
                ("timestamp" (timestamp-to-string timestamp))
                #+allegro
                ("hostname"
                 (let ((outputs (excl.osi:command-output "hostname")))
                   (if (and outputs (stringp (car outputs)))
                     (car outputs))))))
          (loop for r1 being the hash-values of test-reports
              if r1 collect r1 into reports-list
              finally
                (loop for report in reports-list do
                      (when report
                        (write report :stream s)
                        (pprint-newline :mandatory s))))
          (with-xml-tagged-pprint-logical-block (s "system-out")
            (with-pprint-cdata (s)
              (nst-dump :stream s)))
          (pprint-newline :mandatory s)
          (with-xml-tagged-pprint-logical-block (s "system-err")
            (with-pprint-cdata (s)))))))
  0 *default-xml-pprint-dispatch*)

(set-pprint-dispatch 'check-result
  (named-function pprint-xml-check-result
    (lambda (s item)
      (with-accessors ((group-name check-result-group-name)
                       (check-name check-result-check-name)
                       (info check-result-info)
                       (failures check-result-failures)
                       (errors check-result-errors)
                       (elapsed-time check-result-elapsed-time)
                       (timestamp result-stats-timestamp)) item
        (let ((*reporting-check-name* check-name))
          (declare (special *reporting-check-name*))

          ;; Open the testcase block.
          (with-xml-tagged-pprint-logical-block
              (s "testcase" :properties
                 (("classname" (symbol-to-junit-name group-name))
                  ("name" check-name)
                  ("time" (elapsed-time-to-string elapsed-time))
                  ("timestamp" (timestamp-to-string timestamp))))

            ;; Contents of the testcase block.
            (when errors
              ;; Print the errors in a vertical list.
              (loop for (error-note . others) on errors do
                    ;; (format-error-report-junit-xml s check-name error-note)
                    (write error-note :stream s)
                    (when others (pprint-newline :mandatory s))))

            (when (and errors failures)
              (pprint-newline :mandatory s))

            (when failures
              (loop for (failure . others) on failures do
                    ;; (format-failure-report-junit-xml s check-name failure)
                    (let ((*nst-note-type* :failure))
                      (declare (special *nst-note-type*))
                      (write failure :stream s))
                    (when others (pprint-newline :mandatory s))))
            (when info
              (with-xml-tagged-pprint-logical-block (s "system-out")
                (with-pprint-cdata (s)
                  (loop for (note . others) on info do
                    (let ((*note-type* :info))
                      (declare (special *note-type*))
                      (write note :stream s))
                    (when others (pprint-newline :mandatory s)))
                  )))
            )))))
  0 *default-xml-pprint-dispatch*)

(set-pprint-dispatch 'error-check-note
  (named-function pprint-xml-error-check-note
    (lambda (s error-note)
      (let ((check-name (cond ((boundp '*reporting-check-name*)
                               (symbol-value '*reporting-check-name*))
                              (t 'test-unspecified))))
        (with-accessors ((error error-check-note-error)) error-note
          (pprint-logical-block (s '(1 2))
            (with-xml-tagged-pprint-logical-block
                (s "error" :properties
                   (("message" (format nil "~a raised an error: ~a"
                                 (symbol-to-junit-name check-name)
                                 (symbol-to-junit-name (type-of error))))
                    ("type" (symbol-to-junit-name (type-of error)))))
              (with-pprint-cdata (s)
                (princ "Lisp backtrace (within NST context):  " s)
                (let ((backtrace #+allegro (error-check-note-zoom error-note)
                                 #-allegro nil))
                  (cond
                    (backtrace
                     (loop for (bt . others) on backtrace do
                           (format s "~a" bt)
                           (when others (format s "~%"))))
                    (t (princ "expression top-level" s)))))))))))
  0 *default-xml-pprint-dispatch*)

(defmethod check-note-type-string (type (context (eql :xml)))
  (case type
    ((:failure) "failure")
    ((:error) "error")
    ((:warning) "warning")
    ((:info) "info")))

(set-pprint-dispatch 'check-note
  (named-function pprint-xml-check-note
    (lambda (s check-note)
      (let* ((note-type (cond
                          ((not (boundp '*note-type*)) :failure)
                          (t (symbol-value '*note-type*)))))
        (with-accessors ((context check-note-context)) check-note
          (with-xml-tagged-pprint-logical-block
              (s (check-note-type-string note-type :xml) :properties
                 (("message" (with-output-to-string (str)
                               (apply-check-note-formatter str check-note)))
                  ("type" (format nil "lisp.nst.criterion.~a"
                            (get-local-criterion-context context)))))
            (with-pprint-cdata (s)
              (pprint-logical-block (s context)
                (loop for ct in context do
                      (format s "~a~%" ct)))))))))
  1 *default-xml-pprint-dispatch*)
