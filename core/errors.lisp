;;; File errors.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015, 2016 John Maraist
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

;;;
;;; Error conditions.
;;;

(define-condition nst-error () ())

(defmacro define-nst-error (name fields (stream exp) &body printer)
  `(progn
     (define-condition ,name (nst-error) ,fields
                       (:report (lambda (,exp ,stream) ,@printer)))
     (set-pprint-dispatch ',name (lambda (,stream ,exp) ,@printer))))

(define-nst-error no-nst-groups-in-package
    ((package :initarg :package :reader package-of))
  (stream exp)
  (format stream "No NST packages in package ~s" (package-of exp)))

(define-nst-error no-such-nst-group
    ((group :initarg :group :reader group))
  (stream exp)
  (format stream "No such NST group ~s" (group exp)))

(define-nst-error no-such-nst-test
    ((group :initarg :group :reader group)
     (test :initarg :test :reader test))
  (stream exp)
  (format stream "No such NST test ~s in group ~s" (test exp) (group exp)))

(define-nst-error criterion-missing-mandatory-argument
    ((criterion-name  :initarg :criterion-name  :reader criterion-name)
     (required-keyarg :initarg :required-keyarg :reader required-keyarg))
  (stream cnd) (format stream "Criterion ~s requires ~s argument"
                 (criterion-name cnd) (required-keyarg cnd)))

(define-nst-error not-expected-form
    ((expected-form :initarg :expected-form :reader expected-form)
     (actual-value :initarg :actual-value :reader actual-value))
  (stream cnd) (format stream "Expected ~a, got ~s"
                 (expected-form cnd) (actual-value cnd)))

;;; -----------------------------------------------------------------

(define-condition nst-deprecation-warning-mixin ()
  ((old-name :reader old-name :initarg :old-name)
   (replacement :reader replacement :initarg :replacement))
  (:documentation "Mixin of field used in deprecation warnings"))

(define-condition nst-hard-deprecation (warning
                                        nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream)
             (pprint-logical-block (stream '(1 2))
               (princ (old-name cnd) stream)
               (princ " is deprecated and MAY NOT OPERATE CORRECTLY; use "
                      stream)
               (let ((replacement (replacement cnd)))
                 (cond
                   ((listp replacement)
                    (format stream "one of ~{~a~^, ~:_~}" replacement))
                   (t
                    (format stream "~a" replacement))))
               (princ " instead." stream)))))

(defun soft-dep-warning (prefix cnd stream)
  (format stream
      "~a~a is deprecated~a."
    prefix (old-name cnd)
    (let ((repl (replacement cnd)))
      (cond
       ((null repl) ", ignored, and will be removed in a future release")
       ((not (listp repl)) (format nil "; use ~a instead" repl))
       ((eql (length repl) 1) (format nil "; use ~a instead" (car repl)))
       (t (with-output-to-string (buf)
            (format buf "; use one of ")
            (loop for (first . others) on repl do
              (format buf "~a" first)
              (when others (princ " " buf)))
            (format buf " instead")))))))

(define-condition nst-soft-deprecation
    (style-warning nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream) (soft-dep-warning "" cnd stream))))

(define-condition nst-soft-keyarg-deprecation
    (style-warning nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream) (soft-dep-warning "Argument " cnd stream))))

;;; -----------------------------------------------------------------

(defmacro with-nst-control-handlers
    (((formal flag &key
              (for-fail nil) (for-error t)
              (cerror-label-var 'exit-tests-label)
              (cerror-label nil cerror-label-supp-p)
              (with-retry nil with-retry-supp-p)
              (post-cerror nil post-cerror-supp-p)
              (group nil) (tests nil) (fail-test-msg "Unspecified failure")
              (log-location nil)
              (handler-return-to nil handler-return-to-supp-p)
              (handler-return nil))
      &body handler)
     &body body)
  "Macro for inserting restarts at points in the NST test-running stack."

  (unless (listp log-location)
    (setf log-location (list log-location)))
  (let* ((handler-body-fn (gensym "handler-body"))
         (core
          `(flet ((,handler-body-fn (,formal ,flag)
                    ,@(when log-location
                        `((let ((the-log-location (list ,@log-location)))
                            (destructuring-bind (location &key group test tests)
                                the-log-location
                              (setf group (cond
                                            (group (format nil ", group ~a"
                                                     (group-record-name group)))
                                            (t "")))
                              (setf test (cond (test (format nil ", test ~a"
                                                       (test-record-name test)))
                                               (t "")))
                              (setf tests
                                (cond
                                  (tests (format nil ", tests~{ ~a~^,~}"
                                           (mapcar #'test-record-name
                                                   tests)))
                                  (t "")))
                              (format-at-verbosity 4 "Failure ~s~a~a~a"
                                                 location group test tests)))))
                    ,@(when tests
                        ;; If the body fails, record the failure
                        ;; against each test being run.
                        `((loop for test-record in ,tests do
                                (setf (gethash (test-record-results test-record)
                                               +results-record+)
                                  (make-config-error ,formal
                                                     ,group test-record
                                                     ,fail-test-msg)))))
                    ,@handler
                    (when ,flag
                      (cerror ,cerror-label-var ,formal)
                      ,@(when post-cerror-supp-p `(,post-cerror)))
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
                     `((error (named-function nst-control-for-error
                                (lambda (,formal)
                                  (funcall #',handler-body-fn
                                           ,formal *debug-on-error*)))))))
              ,@body))))

    (when cerror-label-supp-p
      (setf core `(let ((,cerror-label-var ,cerror-label)) ,core)))
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
