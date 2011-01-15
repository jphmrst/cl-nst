;;; File elementdef.lisp
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

(in-package :defdoc)

(defvar *default-element-class* 'standard-doc-element)
(defgeneric get-element-class (package name spec forms)
  (:method (package name spec forms)
     (declare (ignore package name spec forms))
     *default-element-class*))

(defgeneric compile-element (package spec args)
  (:method (package spec args)
     (cond
      ((stringp args)
       (compile-string-element package spec args))
      ((and (eql (length args) 1) (stringp (car args)))
       (compile-string-element package spec (car args)))
      ((and (listp args) (symbolp (car args)))
       (compile-symbol-headed-element (car args) package spec (cdr args)))
      ((listp args)
       (get-element-aggregation
        package spec (mapcar (named-function compile-element-mapper
                               (lambda (x)
                                 (compile-element package spec x)))
                             args)))
      (t
       (error "Odd spec element ~s" args)))))

(defgeneric compile-string-element (package spec string)
  (:method (package spec string)
     (compile-symbol-headed-element (string-implicit-symbol-head package
                                                                 spec string)
                                    package spec (list string))))

(defgeneric string-implicit-symbol-head (package spec string)
  (:method (package spec string)
     (declare (ignore package spec string))
     :plain))

(defgeneric get-element-aggregation (package spec elements)
  (:method (package spec elements)
     (declare (ignore package spec))
     (make-instance 'standard-paragraph-list :paragraphs elements)))

(defgeneric compile-symbol-headed-element (hd package spec args)
  (:method (hd package spec args)
     (declare (ignore package spec))
     (error "Unrecognized element specifier: (~s~{ ~s~})" hd args)))

(defclass standard-doc-element ()
     ())

(defmacro def-element (name (new-class &key (class 'standard-doc-element)
                                       (package (gensym) package-supp-p)
                                       (spec (gensym) spec-supp-p)
                                       (arg-list (gensym) arg-list-supp-p)
                                       (args nil args-supp-p))
                            slots &body body)
  (let ((declares (cond
                    ((eq 'declare (caar body))
                     (cdr (pop body)))
                    (t nil))))
    (unless package-supp-p
      (push `(ignore ,package) declares))
    (unless spec-supp-p
      (push `(ignore ,spec) declares))
    (unless (or args-supp-p arg-list-supp-p)
      (push `(ignore ,arg-list) declares))
    (when args-supp-p
      (setf body `((destructuring-bind ,args ,arg-list ,@body))))
    `(progn
       (defclass ,new-class (,class) ,slots)
       (defmethod compile-symbol-headed-element ((hd (eql ',name))
                                                 ,package ,spec ,arg-list)
         ,@(when declares `((declare ,@declares)))
         ,@body))))
