;;; File method.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
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

(defun check-result-union (&rest reports)
  (let ((result (cond
                  ((null reports) (make-and-calibrate-check-result))
                  (t (pop reports)))))
    (when reports
      (with-slots ((result-warnings warnings)
                   (result-failures failures)
                   (result-errors errors)
                   (result-info info)) result
        (loop for report in reports do
              (with-slots ((this-warnings warnings)
                           (this-failures failures)
                           (this-errors errors)
                           (this-info info)) report
                (setf result-warnings (nconc result-warnings this-warnings)
                      result-failures (nconc result-failures this-failures)
                      result-errors (nconc result-errors this-errors)
                      result-info (nconc result-info this-info)))))
      (calibrate-check-result result))
    result))

(define-method-combination nst-results :operator check-result-union)
(def-documentation (method-combination nst-results)
    (:tags object)
    (:intro "Method combination for unifying NST result records returned by different methods."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-methods* (make-hash-table :test 'eq)
    "Global variable for programmer-defined NST test methods."))

(defmacro def-test-generic (function-name &body forms)
  (multiple-value-bind (documentation method-combination)
      (decode-def-test-generic-body forms)
    (let ((arg (gensym))
          (use-combination (cond
                            (method-combination method-combination)
                            (t 'nst-results))))
      `(progn
         (defgeneric ,function-name (,arg)
           ,@(when documentation `((:documentation ,documentation)))
           ,@(unless (eq use-combination t)
               `((:method-combination ,use-combination))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',function-name *test-methods*) ',use-combination))
         ',function-name))))
(def-documentation (compiler-macro def-test-generic)
    (:tags primary)
    (:short "Declare a generic test function."))

(defun decode-def-test-generic-body (forms)
  (let ((documentation)
        (doc-supp-p nil)
        (method-combination)
        (method-combination-supp-p nil))
    (loop for form in forms do
      (unless (consp form)
        (error "Expected cons, got ~s" form))
      (let ((form-head (car form)))
        (cond
          ((eq form-head :documentation)
           (when doc-supp-p
             (error "Multiple documentation strings in def-test-generic"))
           (setf documentation (cadr form) doc-supp-p t))
          ((eq form-head :method-combination)
           (when method-combination-supp-p
             (error "Multiple method combinations in def-test-generic"))
           (setf method-combination (cadr form) method-combination-supp-p t))
          (t
           (error "~s not expected in def-test-generic" form-head)))))
    (values documentation method-combination)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass method-context-layer (context-layer)
     ((method-name :initarg :method-name :accessor method-name)
      (class-name :initarg :class-name :accessor class-name)
      (object :initarg :object :accessor object))
  (:documentation "A record of method-based test invocation."))

(set-pprint-dispatch 'method-context-layer
  #'(lambda (s cl)
      (with-accessors ((method-name method-name)
                       (class-name class-name)
                       (object object)) cl
        (cond
         ((> *nst-verbosity* 2)
          (format s "calling test method ~a on class ~a on ~s"
            method-name class-name object))
         (t
          (format s "calling test method ~a on class ~a"
            method-name class-name))))))

(defmacro with-method-context-layer ((method-name class-name object) &body body)
  `(with-context-layer (make-instance 'method-context-layer
                         :method-name ',method-name
                         :class-name ',class-name
                         :object ,object)
     ,@body))

(defmethod show-context-layer ((layer method-context-layer))
  (declare (special -context-display-state-))
  (setf (gethash 'criterion -context-display-state-) t)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-test-method (function-name (arg class) &body body)
  (let ((documentation (when (and body (stringp (car body)))
                         (pop body)))
        (combination (gethash function-name *test-methods*)))
    `(progn
       (defmethod ,function-name ,@(unless (eq combination t)
                                     `(,combination)) ((,arg ,class))
         ,@(when documentation `(,documentation))
         (let ((result (with-method-context-layer (,function-name ,class ,arg)
                         ,@body)))
           (push ,(format nil "Checked test method ~s for ~s"
                    function-name class)
                 (check-result-info result))
           result))
       ',function-name)))
(def-documentation (compiler-macro def-test-method)
    (:tags primary)
    (:short "Define one method for a generic test function."))

(defmacro def-test-method-criterion (function-name class documentation
                                                   &optional
                                                   (criterion nil crit-supp-p))
  (unless crit-supp-p
    (setf criterion documentation
          documentation nil))
  (let ((arg (gensym)))
    `(def-test-method ,function-name (,arg ,class)
       ,@(when documentation `(,documentation))
       (check-criterion-on-form ',criterion `(list ,,arg)))))

(defun invoke-test-methods (obj)
  (apply #'check-result-union
         (loop for method-name being the hash-keys of *test-methods*
               for method-fn = (symbol-function method-name)
               for actual-method
                 = (when method-fn
                     (closer-mop:compute-applicable-methods-using-classes
                      method-fn (list (find-class (type-of obj)))))
               if actual-method
                 collect (funcall method-fn obj))))

(def-criterion (:methods () (object))
  (invoke-test-methods object))
