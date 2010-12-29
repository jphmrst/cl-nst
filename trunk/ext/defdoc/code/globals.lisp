;;; File globals.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
(in-package :defdoc)

(defvar *docstring-style* 'standard-docstring-style)
(defvar *sectioning-level* 1)

(defgeneric format-doc (stream style spec))

(defgeneric format-docspec (stream style spec type)
  (:method :around (stream style spec type)
     (cond ((symbolp style)
            (format-docspec stream (make-instance style)
                            spec type))
           (t (call-next-method))))
  (:method (stream style spec type)
     (format-docspec-element style type spec stream)))

(defgeneric format-docspec-element (style target-type element stream))

(define-condition option-without-required-option (warning)
  ((given :initarg :given :reader given)
   (missing :initarg :missing :reader missing)
   (def-type :initarg :def-type :reader def-type)
   (name :initarg :name :reader name))
  (:report
   (lambda (warning stream)
     (with-accessors ((given given) (missing missing)
                      (def-type def-type) (name name)) warning
       (format stream
           "Option ~s ~:_but ~:_not ~:_option ~:_~s ~:_given ~:_in ~:_~a ~s"
         given missing def-type name)))))

(defmacro def-slot-supp-predicates (class slot-and-function-specs)
  `(progn
     ,@(loop for (fname slot) in slot-and-function-specs
             collect
             `(defgeneric ,fname (o)
                (:method (o) (declare (ignore o)) nil)
                (:method ((o ,class)) (slot-boundp o ',slot))))))

(defvar *defdoc-debuggable* t)
(defvar *defdoc-debug* nil)
(defmacro defdoc-debug (format &rest args)
  (when *defdoc-debuggable*
    `(when *defdoc-debug*
           (format t ,format ,@args))))

(define-condition unrecognized-deflabel-form (error)
    ((head :initarg :head :reader head)
     (args :initarg :args :reader args)
     (label-name :initarg :label-name :reader label-name))
  (:report (lambda (warning stream)
             (with-accessors ((head head) (args args)
                              (label-name label-name)) warning
               (format stream "Unrecognized form (~a ~{ ~s~}) in def-label of ~s"
                       head args label-name)))))

