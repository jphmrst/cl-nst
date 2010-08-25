;;; File check.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2010 Smart Information Flow Technologies.
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

(defgeneric apply-criterion (top args form)
  (:documentation
   "Internal generic function whose methods are the translations of criteria."))

(defmacro returning-criterion-config-error ((msg) &body forms)
  "For use within criteria definitions only --- catch errors and process them
as errors in the \"glue\" among criteria and forms."
  `(handler-bind-interruptable
       ((error
         #'(lambda (error)
             (unless *debug-on-error*
               (return-from apply-criterion
                 (make-error-report error
                                    :format (format nil "Criterion error: ~a"
                                              ,msg)))))))
     ,@forms))

(defmacro returning-test-error (&body forms)
  "For use within criteria definitions only --- catch errors and process them
as errors arising from within the ."
  `(handler-bind-interruptable ((error #'(lambda (e)
                                           (unless *debug-on-error*
                                             (return-from apply-criterion
                                               (make-error-report e))))))
     ,@forms))

(defmethod apply-criterion :around (top args form)
  (format-at-verbosity 3
      "Applying criterion ~s~{ ~s~}~%  to ~s~%" top args form)
  (let ((*nst-context* (cons (make-context-layer :criterion top
                                                 :criterion-args args
                                                 :given-stack form)
                             *nst-context*)))
    (declare (special *nst-context*))
    (let ((result (returning-test-error (call-next-method))))
      (format-at-verbosity 3 "  Result at ~s is ~s~%" top result)
      result)))

(defun extract-parameters (x)
  (loop for item in x
        append (cond
                 ((listp x) (extract-parameters x))
                 ((and (symbolp x)
                       (eql (symbol-package x) (find-package :common-lisp)))
                  nil)
                 (t (list x)))))

;;; (defmacro continue-check (criterion form)
;;;   (warn "continue-check is deprecated; use check-criterion-on-value or check-criterion-on-form within def-criterion or def-criterion-unevaluated")
;;;   `(continue-check ,criterion ,form))
(defun continue-check #|continue-check-actual|# (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

(defun check-criterion-on-value (criterion expr)
  "Verify that a value adheres to a criterion."
  (check-criterion-on-form criterion `(list ',expr)))
(defun check-criterion-on-form (criterion form)
  "Verify that an unevaluated form adheres to a criterion."
  (unless (listp criterion)
    (setf criterion (list criterion)))
  (apply-criterion (car criterion) (cdr criterion) form))

(defun build-continue-check-expr (criterion form)
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

;; #+allegro (excl::define-simple-parser def-criterion caadr :nst-criterion)
(defmacro def-criterion ((name args-formals values-formals) &body forms)
  "Define a new criterion for use in NST tests.

  \(def-criterion \(NAME CRITERION-LAMBDA-LIST
                       VALUES-LAMBDA-LIST)
    [ DOCUMENTATION ]
    FORM
    FORM
    ...
    FORM)"

  (let ((fp (gensym "values-form"))
        (ap (gensym "args"))
        (vs (gensym "values"))
        (docstring nil)
        (form-decls nil))
    (when (stringp (car forms))       (setf docstring (pop forms)))
    (when (eq (caar forms) 'declare)  (setf form-decls (cdr (pop forms))))
    `(progn
       #+allegro (excl:record-source-file ',name :type :nst-criterion)
       (defmethod apply-criterion ((top (eql ',name)) ,ap ,fp)
         (declare (optimize (debug 3)) ,@form-decls)
         ,@(when docstring (list docstring))
         (returning-criterion-config-error
             ((format nil "Criterion arguments ~a do not match lambda-list ~a"
                ,ap ',args-formals))
           (destructuring-bind ,args-formals ,ap
             (let ((,vs (returning-test-error (eval ,fp))))
               (returning-criterion-config-error
                   ((format nil
                        "Values under test ~a do not match lambda-list ~a"
                      ,vs ',values-formals))
                 (destructuring-bind ,values-formals (eval ,fp)
                   (returning-criterion-config-error
                       (,(format nil "Error from criterion ~s body" name))
                     ,@forms)))))))
       ,@(when docstring
           `((setf (documentation ',name :nst-criterion) ,docstring))))))

;; #+allegro (excl::define-simple-parser def-criterion-unevaluated
;;              caadr :nst-criterion)
(defmacro def-criterion-unevaluated ((name args-formals forms-formal &key
                                           (ignore-forms nil))
                                     &body forms)
  "Define a new criterion for use in NST tests.

  \(def-criterion-unevaluated \(NAME CRITERION-LAMBDA-LIST
                                   FORMS-ARG)
    [ DOCUMENTATION ]
    FORM
    FORM
    ...
    FORM)"

  (let ((ap (gensym "args"))
        (docstring nil)
        (form-decls nil))
    (when (stringp (car forms))       (setf docstring (pop forms)))
    (when (eq (caar forms) 'declare)  (setf form-decls (cdr (pop forms))))
    `(progn
       #+allegro (excl:record-source-file ',name :type :nst-criterion)
       (defmethod apply-criterion ((top (eql ',name)) ,ap ,forms-formal)
         (declare (optimize (debug 3)) ,@form-decls
                  ,@(when ignore-forms `((ignore ,forms-formal))))
         ,@(when docstring (list docstring))
         (returning-criterion-config-error
             ((format nil "Criterion arguments ~a do not match lambda-list ~a"
                ,ap ',args-formals))
           (destructuring-bind ,args-formals ,ap
             (returning-criterion-config-error
                 (,(format nil "Error from criterion ~s body" name))
               ,@forms))))
       ,@(when docstring
           `((setf (documentation ',name :nst-criterion) ,docstring))))))

#+allegro (excl::define-simple-parser def-values-criterion caadr :nst-criterion)
(defmacro def-values-criterion ((name args-formals forms-formals &key
                                      (declare nil decl-supp-p))
                                &body forms)
  "DEPRECATED: use def-criterion instead."
  (warn 'style-warning "def-values-criterion is deprecated from 1.3.0.")
  (let ((ap (gensym "args")) (fp (gensym "form")) (vs (gensym "values")))
    `(progn
       (defmethod apply-criterion ((top (eql ',name)) ,ap ,fp)
         (returning-criterion-config-error
             ((format nil
                  ,(format nil "Criterion arguments ~~a do not match lambda-list ~a" args-formals)
                ,ap))
           (destructuring-bind ,args-formals ,ap
             (let ((,vs (returning-test-error (eval ,fp))))
               (returning-criterion-config-error
                   ((format nil
                        ,(format nil "Values under test ~~a do not match lambda-list ~a"
                           forms-formals)
                      ,vs))
                 (destructuring-bind ,forms-formals ,vs
                   (declare (special ,@(extract-parameters forms-formals)))
                   ,@(when decl-supp-p `((declare ,@declare)))
                   (returning-criterion-config-error
                       (,(format nil "Error from criterion ~s body" name))
                     (eval (progn ,@forms))))))))))))

#+allegro (excl::define-simple-parser def-form-criterion caadr :nst-criterion)
(defmacro def-form-criterion ((name args-formals form-formal) &rest forms)
  "DEPRECATED: use def-criterion-unevaluated instead."
  (warn
   "def-form-criterion is deprecated from 1.3.0, AND PROBABLY WILL NOT WORK.")
  (let ((ap (gensym "args")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,form-formal)
       (destructuring-bind ,args-formals ,ap
         (eval (progn ,@forms))))))

(defmacro def-criterion-alias ((name . args-formals) docstring-or-form
                               &optional (form nil form-supp-p))
  "Define one criterion in terms of another.

  \(def-criterion-alias \(name &rest args)
    [ DOCUMENTATION ]
    EXPANSION)"
  (unless form-supp-p (setf form docstring-or-form docstring-or-form nil))
  (let* ((vsf (gensym "form"))
         (expanded (gensym "res"))
         (new-name (gensym "new-name"))
         (new-args (gensym "new-args"))
         (redef `(def-criterion-unevaluated (,name ,args-formals ,vsf)
                     ,@(when docstring-or-form (list docstring-or-form))
                     (let* ((,expanded ,form)
                            (,new-name (cond
                                        ((symbolp ,expanded) ,expanded)
                                        (t (car ,expanded))))
                            (,new-args (cond
                                        ((symbolp ,expanded) nil)
                                        (t (cdr ,expanded)))))
                       (apply-criterion ,new-name ,new-args ,vsf)))))
    (cond
      (docstring-or-form `(progn
                            ,redef
                            ,@(when docstring-or-form
                                `((setf (documentation ',name :nst-criterion)
                                        ,docstring-or-form)))))
      (t redef))))

;;;(defvar *error-checking* nil
;;;  "Criteria such as :check-err set this variable to t (and declare it special)
;;;to suppress error-handling in continue-check, and thus become able to handle
;;;all further errors themselves.")

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
