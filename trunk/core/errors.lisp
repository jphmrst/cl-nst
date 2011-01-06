;;; File errors.lisp
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
             (format stream "~@<~a is deprecated and MAY NOT OPERATE CORRECTLY~
                             ; use ~:[~a~;one of ~{~a~^, ~:_~}~] instead.~:>"
               (old-name cnd) (listp (replacement cnd)) (replacement cnd)))))

(defun soft-dep-warning (prefix cnd stream)
  (format stream
      "~a~a is deprecated~a."
    prefix (old-name cnd)
    (let ((repl (replacement cnd)))
      (cond
       ((null repl) ", ignored, and will be removed in a future release")
       ((not (listp repl)) (format nil "; use ~a instead" repl))
       ((eql (length repl) 1) (format nil "; use ~a instead" (car repl)))
       (t (format nil "; use one of ~{~a~^, ~} instead" repl))))))

(define-condition nst-soft-deprecation
    (style-warning nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream) (soft-dep-warning "" cnd stream))))

(define-condition nst-soft-keyarg-deprecation
    (style-warning nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream) (soft-dep-warning "Argument " cnd stream))))

