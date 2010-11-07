;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
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

(defvar +label-defs+ (make-hash-table :test 'eq))

(defgeneric get-compiled-labeldef (package name options forms))
(defvar *label-compiler* #'get-compiled-labeldef)

(defmacro def-property-label (name options &body body)
  (let ((p (gensym))
        (spec (gensym)))
    `(let* ((,p (symbol-package ',name))
            (,spec (funcall *label-compiler* ,p ',name ',options ',body)))
        (setf (gethash ',name +label-defs+) ,spec)
        ',name)))

(defun get-labeldef (name)
  (gethash name +label-defs+))

;;; -----------------------------------------------------------------

(defvar *label-class* 'standard-doc-label)
(defgeneric get-label-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *label-class*))

(defclass doc-label ()
     ((label-name :initarg :name :reader doc-label-name)))

(defclass standard-doc-label (doc-label)
     ((default-subsort :accessor standard-label-default-subsort)))

(defgeneric has-subsort-label (label-def)
  (:method (label-def) (and (typep label-def 'standard-doc-label)
                            (slot-boundp label-def 'default-subsort))))

(defmethod get-compiled-labeldef (package name options forms)
  (declare (ignore options))
  (let* ((use-class (get-label-class package name forms))
         (result (make-instance use-class :label-name name)))
    (loop for form in forms do
          (let ((hd (car form))
                (tl (cdr form)))
            (process-standard-labeldef-form result package hd tl)))
    result))

(defgeneric process-standard-labeldef-form (labeldef package
                                                     form-head form-args)
  (:method ((labeldef standard-doc-label) package form-head form-args)
     (declare (ignore package))
     (case form-head
       ((:default-subsort)
        (destructuring-bind (subsort) form-args
          (setf (slot-value labeldef 'default-subsort) subsort)))
       (otherwise (error "Unrecognized form (~a~{ ~s~}) in def-label of %s"
                         form-head form-args (doc-label-name labeldef))))))

