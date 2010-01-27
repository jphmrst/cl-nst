;;; File errors.lisp
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

(define-condition nst-soft-deprecation (style-warning
                                        nst-deprecation-warning-mixin) ()
  (:report (lambda (cnd stream)
             (format stream
                 "~a is deprecated; use ~:[~a~;one of ~{~a~^, ~:_~}~] instead."
               (old-name cnd) (listp (replacement cnd)) (replacement cnd)))))

