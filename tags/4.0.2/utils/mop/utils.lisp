;;; File utils.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2012 Smart Information Flow Technologies.
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
(in-package :nst-mop-utils)

(defmacro def-derived-criterion (criterion &key
                                 (class nil class-supp-p)
                                 (slot-keys nil)
                                 (ignore-slots nil)
                                 ;; (superclass-criterion
                                 ;;  nil superclass-criterion-supp-p)
                                 )
  (cond
    (class-supp-p
     (let ((slot-key-lookup (let ((result (make-hash-table :test 'eq)))
                              (loop for (slot key) in slot-keys do
                                (setf (gethash slot result) key))
                              result))
           (keyarg (gensym "keyargs"))
           (localvar (gensym "o"))
           (class-object (find-class class))
           (slots-to-check)
           (supp-p-var (make-hash-table :test 'eq))
           (slot-accessor-expressions (make-hash-table :test 'eq))
           (slot-boundp-expressions (make-hash-table :test 'eq)))

       (loop for slot in (compute-slots class-object) do
         (with-accessors ((name slot-definition-name)
                          (initargs slot-definition-initargs)) slot
           (unless (member name ignore-slots)
             (push name slots-to-check)

             (unless (gethash name slot-key-lookup)
               (let ((use-keyarg
                      (cond (initargs (intern (symbol-name (car initargs))))
                            (t (symbol-name name)))))
                 (setf (gethash name slot-key-lookup) use-keyarg)))

             (let ((the-supp (gensym (concatenate 'string
                                       (symbol-name name) "-supp-p")))
                   (the-accessor `(lambda (,localvar)
                                      (slot-value ,localvar ',name)))
                   (the-boundp   `(lambda (,localvar)
                                      (slot-boundp ,localvar ',name))))
               (setf (gethash name supp-p-var) the-supp
                 (gethash name slot-accessor-expressions) the-accessor
                 (gethash name slot-boundp-expressions) the-boundp)))))

       (let* ((initial-predicate
               `(:predicate (lambda (obj) (typep obj ',class)))))

         `(nst:def-criterion-alias
              (,criterion &rest ,keyarg
                          &key ,@(loop for slot in slots-to-check collect
                                       `(,(gethash slot slot-key-lookup)
                                         nil ,(gethash slot supp-p-var)))
                          &allow-other-keys)
              (let ((slot-checkers))
                ,@(loop for slot in slots-to-check collect
                        `(when ,(gethash slot supp-p-var)
                           (push (list :apply ;; ,slot
                                       ',(gethash slot
                                                  slot-accessor-expressions)
                                       ,(gethash slot slot-key-lookup)
                                       )
                                 slot-checkers)))
                `(:and ,',initial-predicate ,@slot-checkers))))))

    (t (error "Must provide :class error for basis class of criterion."))))
