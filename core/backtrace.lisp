;;; File backtrace.lisp
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

#-(or allegro ecl sbcl cmucl)
(defun get-backtrace-string-lines (e) nil)

#+(or allegro ecl sbcl cmucl)
(defun get-backtrace-string-lines (e)
  (string-lines (trivial-backtrace:print-backtrace e :output nil)))

(defun string-lines (s)
  (let ((ending (length s))
        (breaks ()))
    (loop while ending do
      (push ending breaks)
      (setf ending (position #\Newline s :from-end t :start 0 :end ending)))
    (push -1 breaks)
    (loop for (before after) on breaks while (and before after)
          collect
          (subseq s (+ before 1) after))))

;;; (defmacro make-backtrace-lines ()
;;;   `(block backtrace-maker
;;;      (let ((raw (with-output-to-string (stream)
;;;                   (let ((top-level:*zoom-print-circle* nil)
;;;                         (*print-right-margin* 1000000))
;;;                     (declare (special top-level:*zoom-print-circle*
;;;                                       *print-right-margin*))
;;;                     (top-level.debug:zoom stream :function nil :verbose nil
;;;                                           :moderate t :specials nil
;;;                                           :length 5 :level nil)))))
;;;
;;;        (handler-bind ((error #'(lambda (cnd)
;;;                                  (declare (ignorable cnd))
;;;                                  (format-at-verbosity 3
;;;                                      "Caught error formatting backtrace~%")
;;;                                  (return-from backtrace-maker
;;;                                    (list "Caught error while formatting backtrace, returning raw lines"
;;;                                          raw)))))
;;;          (let ((lines (loop for spot = (position #\Newline raw)
;;;                           while spot
;;;                           collect (string-left-trim " ->" (subseq raw 0 spot))
;;;                           do (setf raw (subseq raw (+ 1 spot))))))
;;;            (unless (search ,(if (string= "zz"
;;;                                          (symbol-name 'zz))
;;;                               "emit-error "
;;;                               "EMIT-ERROR ") (car lines))  (pop lines))
;;;            (unless (search ,(if (string= "zz"
;;;                                          (symbol-name 'zz))
;;;                               "emit-error "
;;;                               "EMIT-ERROR ") (car lines))  (pop lines))
;;;
;;;            (let ((orig-lines (loop for line in lines collect line)))
;;;
;;;              (handler-bind
;;;                  ((error #'(lambda (cnd)
;;;                              (declare (ignorable cnd))
;;;                              (format-at-verbosity 3
;;;                                  "Caught error ~s identifying backtrace core~%"
;;;                                cnd)
;;;                              (return-from backtrace-maker
;;;                                (list* "Caught error while identifying backtrace core, returning raw lines"
;;;                                       lines)))))
;;;                (loop while (and lines
;;;                                 (not (search ,(if (string= "zz"
;;;                                                            (symbol-name 'zz))
;;;                                                 "emit-error "
;;;                                                 "EMIT-ERROR ")
;;;                                              (car lines))))
;;;                    do (pop lines))
;;;                (cond
;;;                 ;; We found the "emit-error" line, and it's not at the
;;;                 ;; top of the list of lines.
;;;                 (lines (pop lines))
;;;
;;;                 ;; There is no "emit-error" line, so restore the
;;;                 ;; original list of lines.
;;;                 (t (setf lines orig-lines)))
;;;                (if (search ,(if (string= "zz" (symbol-name 'zz))
;;;                               ":internal"
;;;                               ":INTERNAL") (car lines)) (pop lines))
;;;                (loop while (search ,(if (string= "zz" (symbol-name 'zz))
;;;                                       "core-run-test"
;;;                                       "CORE-RUN-TEST")
;;;                                    (car lines))
;;;                    do (pop lines))
;;;                (let ((first
;;;                       (position-if #'(lambda (x)
;;;                                        (search ,(if (string= "zz"
;;;                                                              (symbol-name 'zz))
;;;                                                     "core-run-test"
;;;                                                     "CORE-RUN-TEST") x))
;;;                                          lines)))
;;;                  (setf lines (subseq lines 0 first)))
;;;                lines)))))))
