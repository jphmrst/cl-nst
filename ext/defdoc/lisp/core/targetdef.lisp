;;; File targetdef.lisp
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

;;; -----------------------------------------------------------------
;;; Global hashtable storing information about the kinds of things we
;;; can declare documentation about.

(defclass standard-doc-target ()
  ((name :initarg :name :accessor name)
   (capitalized :initform nil :initarg :capitalized
                :accessor capitalized-target-name)
   (lower-case  :initform nil :initarg :lower-case
                :accessor lower-case-target-name)
   (docstring-installer :initarg :docstring-installer
                        :accessor docstring-installer)
   (symbol-definition-checker :initarg :symbol-definition-checker
                              :accessor symbol-definition-checker)))
(defmethod initialize-instance :after ((target standard-doc-target)
                                       &key &allow-other-keys)
  (with-accessors ((name name) (capitalized capitalized-target-name)
                   (lower-case lower-case-target-name)) target
                   (unless lower-case
                     (setf lower-case (string-downcase (symbol-name name))))
                   (unless capitalized
                     (setf capitalized (copy-seq lower-case))
                     (setf (elt capitalized 0)
                           (char-upcase (elt capitalized 0))))))

(defvar +doc-target-types+ (make-hash-table :test 'eq)
  "Master global hashtable of all documentation target specifiers.")

(defun get-target-type-docspecs (sym)
  (when (gethash sym +defdocs+)
    (loop for spec being the hash-values of (gethash sym +defdocs+)
          collect spec)))

(defun get-doc-target-types (&optional (sym nil sym-supp-p))
  (cond
   (sym-supp-p
    (loop for type being the hash-keys of +defdocs+ using (hash-value hash)
        if (gethash sym hash) collect type))
   (t
    (loop for type being the hash-keys of +defdocs+ collect type))))

(defun get-target-type (type &optional noerror)
  (let ((type-info (gethash type +doc-target-types+)))
    (unless (or type-info noerror)
      (error "No such documentation type ~s" type))
    type-info))

;;; -----------------------------------------------------------------

(defmacro def-target-type (name (&key (class 'standard-doc-target)
                                      (capitalized nil capitalized-supp-p)
                                      (lower-case nil lower-case-supp-p)
                                      symbol-definition-nocheck
                                      (symbol-definition-checker
                                       nil symbol-definition-checker-supp-p))
                                &body forms)
  (cond
    ((and symbol-definition-nocheck symbol-definition-checker-supp-p)
     (warn "Set :symbol-definition-nocheck for target type ~s; ignoring :symbol-definition-checker"
           name))

    (symbol-definition-nocheck
     (setf symbol-definition-checker `(lambda (e)
                                          (declare (ignore e))
                                          nil)
           symbol-definition-checker-supp-p t))

    ((not symbol-definition-checker-supp-p)
     (warn ":symbol-definition-checker for ~a not defined" name)
     (setf symbol-definition-checker `(lambda (e)
                                          (declare (ignore e))
                                          nil)
           symbol-definition-checker-supp-p t)))

  (multiple-value-bind (lisp-installer lisp-installer-supp-p)
      (decode-doctype-forms forms)
    `(let ((target-spec
            (make-instance ',class
              :name ',name
              :symbol-definition-checker #',symbol-definition-checker
              ,@(when capitalized-supp-p `(:capitalized ,capitalized))
              ,@(when lower-case-supp-p  `(:lower-case ,lower-case)))))
       (setf (docstring-installer target-spec)
             #',(cond
                  (lisp-installer-supp-p lisp-installer)
                  (t '(lambda (x y) (declare (ignore x y))))))
       (unless (gethash ',name +defdocs+)
         (setf (gethash ',name +defdocs+) (make-hash-table :test 'eq)))
       (setf (gethash ',name +doc-target-types+)
             target-spec))))

(defun decode-doctype-forms (forms)
  (let ((lisp-installer nil)
        (lisp-installer-supp-p nil))
    (loop for form in forms do
      (case (car form)
        ((:docstring-installer)
         (destructuring-bind ((name spec) &rest body) (cdr form)
           (setf lisp-installer-supp-p t
                 lisp-installer `(lambda (,name ,spec) ,@body))))))
    (values lisp-installer lisp-installer-supp-p)))

;;; -----------------------------------------------------------------

(defun guess-spec-type (name)
  (cond
    ((fboundp name) 'function)
    ((boundp name) 'variable)
    (t (error "Cannot determine name use for documentation of ~s" name))))

;;; -----------------------------------------------------------------

(define-condition undoc-reporting ()
  ((sym :initarg :symbol :reader sym)
   (usage :initarg :usage :reader usage)))

(define-condition undoc-reporting-defdoc-only (undoc-reporting) ())
(defmethod print-object ((ud undoc-reporting-defdoc-only) stream)
  (format stream "No documentation in DefDoc for ~s as ~a"
    (sym ud) (usage ud)))

(define-condition undoc-reporting-any (undoc-reporting) ())
(defmethod print-object ((ud undoc-reporting-any) stream)
  (format stream "No documentation for ~s as ~a"
    (sym ud) (usage ud)))

(define-condition error-undoc-reporting-defdoc-only
    (undoc-reporting-defdoc-only error) ())
(define-condition warning-undoc-reporting-defdoc-only
    (undoc-reporting-defdoc-only warning) ())
(define-condition style-warning-undoc-reporting-defdoc-only
    (undoc-reporting-defdoc-only style-warning) ())
(define-condition error-undoc-reporting-any
    (undoc-reporting-any error) ())
(define-condition warning-undoc-reporting-any
    (undoc-reporting-any warning) ())
(define-condition style-warning-undoc-reporting-any
    (undoc-reporting-any style-warning) ())

(defun warn-if-undocumented (sym &key error warning defdoc-only)
  (flet ((test-pred (typ)
           (cond
             (defdoc-only (get-doc-spec sym typ))
             (t (or (get-doc-spec sym typ) (documentation sym typ))))))
    (let ((reactor (cond (error #'error) (t #'warn)))
          (reaction-class (cond
                            ((and defdoc-only error)
                             'error-undoc-reporting-defdoc-only)
                            ((and defdoc-only warning)
                             'warning-undoc-reporting-defdoc-only)
                            (defdoc-only
                                'style-warning-undoc-reporting-defdoc-only)
                            (error 'error-undoc-reporting-any)
                            (warning 'warning-undoc-reporting-any)
                            (t 'style-warning-undoc-reporting-any))))
      (loop for typ in (get-doc-target-types) do
        (let ((decl (gethash typ +doc-target-types+)))
          (when (funcall (symbol-definition-checker decl) sym)
            (unless (test-pred typ)
              (funcall reactor reaction-class :symbol sym :usage typ))))))))

