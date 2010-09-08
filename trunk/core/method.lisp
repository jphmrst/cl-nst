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

(define-method-combination nst-results :operator check-result-union
  :documentation "Method combination for unifying NST result records returned
by different methods.")

(defvar *test-methods* (make-hash-table :test 'eq)
  "Global variable for programmer-defined NST test methods.")

(defmacro def-test-generic (function-name &key documentation)
  (let ((arg (gensym)))
    `(progn
       (defgeneric ,function-name (,arg)
         ,@(when documentation `((:documentation ,documentation)))
         (:method-combination nst-results))
       (setf (gethash (symbol-function ',function-name) *test-methods*) t)
       ',function-name)))

(defmacro def-test-method (function-name (arg class) &body body)
  (let ((documentation (when (and body (stringp (car body)))
                         (pop body))))
    `(progn
       (defmethod ,function-name nst-results ((,arg ,class))
         ,@(when documentation `(,documentation))
         (let ((result (progn ,@body)))
           (push ,(format nil "Checked test method ~s for ~s"
                    function-name class)
                 (check-result-info result))
           result))
     ',function-name)))

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
         (loop for method-fn being the hash-keys of *test-methods*
               for actual-method
                 = (closer-mop:compute-applicable-methods-using-classes
                          method-fn (list (find-class (type-of obj))))
               if actual-method
                 collect (funcall method-fn obj))))

(def-criterion (:methods () (object))
  (invoke-test-methods object))
