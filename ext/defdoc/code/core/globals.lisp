;;; File globals.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :defdoc-core)

(defvar *docstring-style* 'defdoc-plaintext:standard-docstring-style)

(defgeneric string-implicit-symbol-head (package spec string))
(defgeneric format-doc (stream style spec &key &allow-other-keys))
(defgeneric format-docspec (stream style spec type &key &allow-other-keys)
  (:method (stream (style symbol) spec type &rest keyargs)
     (apply #'format-docspec stream (make-instance style) spec type keyargs)))

(defgeneric format-docspec-element (style target-type element stream
                                          &key &allow-other-keys))

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

#-allegro
(defmacro named-function (name lambda-expression)
  (declare (ignore name))
  `(function ,lambda-expression))

(defgeneric package-exports-p (package symbol)
  (:method (package symbol)
    (multiple-value-bind (sym status)
        (find-symbol (symbol-name symbol) package)
      (declare (ignore sym))
      (case status
        ((:external) t)
        (otherwise nil)))))
(defgeneric locate-package-home (style target-type spec symbol)
  (:method (style target-type spec symbol)
           (declare (ignore style target-type spec))
           (let ((package (symbol-package symbol)))
             (values package (package-exports-p package symbol)))))
