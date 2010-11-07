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

(defvar +output-frameworks+ (make-hash-table :test 'eq))

(defgeneric get-compiled-output-framework (package name forms))
(defvar *output-compiler* #'get-compiled-output-framework)

(defmacro def-output-framework (name &body body)
  (let ((p (gensym))
        (spec (gensym)))
    `(let* ((,p (symbol-package ',name))
            (,spec (funcall *output-compiler* ,p ',name ',body)))
        (setf (gethash ',name +output-frameworks+) ,spec)
        ',name)))

(defun get-output-framework (name)
  (gethash name +output-frameworks+))

;;; -----------------------------------------------------------------

(defvar *output-framework-class* 'standard-doc-output)
(defgeneric get-output-framework-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *output-framework-class*))

(defclass output-framework ()
     ((name :initarg :name :reader output-framework-name)))

(defclass standard-output-framework (output-framework)
     ())

(defgeneric process-standard-output-framework-form (output-framework package
                                                    form-head form-args)
  (:method ((output-framework standard-output-framework)
            package form-head form-args)
     (declare (ignore package))
     (case form-head
       (otherwise
        (error "Unrecognized form (~a~{ ~s~}) in def-output-framework %s"
               form-head form-args (doc-label-name output-framework))))))

(defmethod get-compiled-output-framework (package name forms)
  (let* ((use-class (get-output-framework-class package name forms))
         (result (make-instance use-class :name name)))
    (loop for form in forms do
      (process-standard-output-framework-form result package
                                                     (car form) (cdr form)))
    result))
