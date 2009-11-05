;;; File check.lisp
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

(defun decode-defcheck-name-and-args (name-or-name-and-args)
  "This function unpacks the information inside the first form of a def-check
block, which can be either a single symbol naming the test, or a list whose
first element is that symbol and whose remaining elements are options."

  (cond
   ((symbolp name-or-name-and-args)
    (return-from decode-defcheck-name-and-args
      (values name-or-name-and-args nil nil nil nil nil nil nil nil)))
   ((listp name-or-name-and-args)
    (destructuring-bind (name &key (setup nil setup-supp-p)
                                   (cleanup nil cleanup-supp-p)
                                   (fixtures nil fixtures-supp-p)
                                   (group nil group-supp-p))
        name-or-name-and-args
      (return-from decode-defcheck-name-and-args
        (values name
                setup setup-supp-p
                cleanup cleanup-supp-p
                fixtures fixtures-supp-p
                group group-supp-p))))
   (t
    (error "~@<Expected symbol or list for def-check argument~_ ~s~:>"
           name-or-name-and-args))))

(defgeneric apply-criterion (top args form))

(defmethod apply-criterion :around (top args form)
  (let ((*nst-context* (cons (make-context-layer :criterion top
                                                 :criterion-args args
                                                 :given-stack form)
                             *nst-context*)))
    (declare (special *nst-context*))
    (call-next-method)))

(defun extract-parameters (x) x)

;;; (defmacro continue-check (criterion form)
;;;   (warn "continue-check is deprecated; use check-subcriterion-on-value or check-subcriterion-on-form within def-criterion or def-criterion-unevaluated")
;;;   `(continue-check ,criterion ,form))
(defun continue-check #|continue-check-actual|# (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

(defun check-subcriterion-on-value (criterion expr)
  (check-subcriterion-on-form criterion `(list ',expr)))
(defun check-subcriterion-on-form (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  (apply-criterion (car criterion) (cdr criterion) form))

(defun build-continue-check-expr (criterion form)
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

#+allegro (excl::define-simple-parser def-criterion caadr :nst-criterion)
(defmacro def-criterion ((name args-formals values-formals) &body forms)
  (let ((fp (gensym "values-form")) (ap (gensym "args")))
    `(progn
       (defmethod apply-criterion ((top (eql ',name)) ,ap ,fp)
         (declare (optimize (debug 3)))
         ;; (format t "Matching ~s~%  to ~s~%" ',ap ',args-formals)
         (destructuring-bind ,args-formals ,ap
           ;; (format t "  done~%Evaluating ~s~%  to match to ~s~%" ,fp ',values-formals)
           (destructuring-bind ,values-formals (eval ,fp)
             ;; (format t "  done~%")
             ,@forms))))))

#+allegro (excl::define-simple-parser def-criterion-unevaluated
              caadr :nst-criterion)
(defmacro def-criterion-unevaluated ((name args-formals forms-formals)
                                     &body forms)
  (let ((ap (gensym "args")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,forms-formals)
       (declare (optimize (debug 3)))
       (destructuring-bind ,args-formals ,ap
         ,@forms))))

#+allegro (excl::define-simple-parser def-values-criterion caadr :nst-criterion)
(defmacro def-values-criterion ((name args-formals forms-formals &key (declare nil decl-supp-p)) &body forms)
  (let ((ap (gensym "args")) (fp (gensym "form")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,fp)
       (destructuring-bind ,args-formals ,ap
         (destructuring-bind ,forms-formals (eval ,fp)
           (declare (special ,@(extract-parameters forms-formals)))
           ,@(when decl-supp-p `((declare ,@declare)))
           (eval (progn ,@forms)))))))

#+allegro (excl::define-simple-parser def-form-criterion caadr :nst-criterion)
(defmacro def-form-criterion ((name args-formals form-formal) &rest forms)
  (let ((ap (gensym "args")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,form-formal)
       (destructuring-bind ,args-formals ,ap
         (eval (progn ,@forms))))))

(defmacro with-criterion-name-args ((name-var formals-var) expr
                                    &body forms)
  (let ((res (gensym)))
    `(let ((,res ,expr) ,name-var ,formals-var)
       (cond
         ((symbolp ,res) (setf ,name-var ,res ,formals-var nil))
         (t (setf ,name-var (car ,res) ,formals-var (cdr ,res))))
       ,@forms)))

(defmacro def-criterion-alias ((name . args-formals) form)
  (let ((vsf (gensym "values-form"))
        (new-name (gensym "new-name")) (new-args (gensym "new-args")))
    `(def-criterion-unevaluated (,name ,args-formals ,vsf)
         (with-criterion-name-args (,new-name ,new-args) ,form
           (apply-criterion ,new-name ,new-args ,vsf)))))

(defvar *error-checking* nil
  "Criteria such as :check-err set this variable to t (and declare it special)
to suppress error-handling in continue-check, and thus become able to handle
all further errors themselves.")

(defmacro within-context ((name args values) &body forms)
  `(let ((*nst-context* (cons (make-context-layer
                               :criterion ',name
                               :criterion-args ',args
                               :given-stack ,(cond
                                              (*nst-context-evaluable*
                                               values)
                                              (t `',values)))
                              *nst-context*)))
     (declare (special *nst-context*))
     ,@forms))


#+allegro (excl::define-simple-parser def-values-check caadr :nst-criterion)
(defmacro def-value-check (&rest args)
  (warn 'nst-soft-deprecation
        :old-name 'def-value-check :replacement 'def-values-criterion)
  `(def-values-criterion ,@args))

#+allegro (excl::define-simple-parser def-control-check caadr :nst-criterion)
(defmacro def-control-check (&rest args)
  (warn 'nst-soft-deprecation
        :old-name 'def-control-check :replacement 'def-form-criterion)
  `(def-form-criterion ,@args))

#+allegro (excl::define-simple-parser def-check-alias caadr :nst-criterion)
(defmacro def-check-alias (&rest args)
  (warn 'nst-soft-deprecation
        :old-name 'def-check-alias :replacement 'def-criterion-alias)
  `(def-criterion-alias ,@args))

(defvar +storage-name-to-test-package+
    (make-hash-table :test 'eq))
