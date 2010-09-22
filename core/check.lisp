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
  (with-criterion-context-layer (:criterion top :criterion-args args :given-stack form)
    ;;(let ((*nst-context* (cons (make-instance 'criterion-context-layer
    ;;                                               :criterion top
    ;;                                               :criterion-args args
    ;;                                               :given-stack form)
    ;;                           *nst-context*)))
    ;;  (declare (special *nst-context*))
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

(defun continue-check #|continue-check-actual|# (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

(defun check-criterion-on-value (criterion expr)
  (check-criterion-on-form criterion `(list ',expr)))
(def-documentation (function check-criterion-on-value)
    (:tags criteria)
    (:short "Verify that a value adheres to a criterion."))
(defun check-criterion-on-form (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  (apply-criterion (car criterion) (cdr criterion) form))
(def-documentation (function check-criterion-on-form)
    (:tags criteria)
    (:short "Verify that an unevaluated form adheres to a criterion."))

(defun build-continue-check-expr (criterion form)
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

(defun decompose-arg-values-lambda-list (args-formals)
  (let ((opt-or-keys nil))
    (flet ((mk-pair (param)
             (cond
              ((symbolp param)
               (let ((form-var (gensym (concatenate 'string
                                         (symbol-name param) "-form"))))
                 (if opt-or-keys
                   (let ((supp-var (gensym (concatenate 'string
                                             (symbol-name param)
                                             "-form-supp-p"))))
                     (list `(,form-var nil ,supp-var)
                           `(,param (when ,supp-var (eval ,form-var)))))
                   (list form-var
                         `(,param (eval ,form-var))))))
              ((listp param)
               (destructuring-bind
                     (param-name &optional
                                 (param-default nil)
                                 (param-supp-var
                                  (gensym (concatenate 'string
                                            (symbol-name param-name)
                                            "-form-supp-p"))))
                   param
                 (let ((form-var (gensym (concatenate 'string
                                           (symbol-name param-name)
                                           "-form-supp-p"))))
                   (if opt-or-keys
                     (list `(,form-var nil ,param-supp-var)
                           `(,param-name
                             (cond (,param-supp-var (eval ,form-var))
                                   (t ,param-default))))
                     (error "~@<Bad element ~s~_in values lambda list ~s~:>"
                            param args-formals)))))
              (t
               (error "~@<Bad element ~s~_in values lambda list ~s~:>"
                      param args-formals)))))
      (loop for param in args-formals
          if (and (symbolp param)
                  (eq (symbol-package param) (find-package :common-lisp)))
          collect (list param nil) into zipper
          and do (when (or (eq param '&optional) (eq param '&key))
                   (setf opt-or-keys t))
          else
          collect (mk-pair param) into zipper
          finally
            (loop for (form-dec val-bnd) in zipper
                  collect form-dec into form-deconstructor
                  if val-bnd
                    collect val-bnd into values-binder
                finally
                  (return-from decompose-arg-values-lambda-list
                    (values form-deconstructor values-binder)))))))

(defmacro def-criterion ((name args-formals values-formals) &body forms)
  (let ((fp)
        (ap (gensym "args"))
        (docstring nil)
        (form-decls nil)
        (args-list-type (cond
                         ((and (consp args-formals)
                               (symbolp (car args-formals))
                               (eq (symbol-package (car args-formals))
                                   (find-package :keyword)))
                          (pop args-formals)
                          ;; (error "This feature is not yet implemented")
                          )
                         (t :forms)))
        (values-list-type (cond
                           ((and (consp values-formals)
                                 (symbolp (car values-formals))
                                 (eq (symbol-package (car values-formals))
                                     (find-package :keyword)))
                            (pop values-formals)
                            ;; (error "This feature is not yet implemented")
                            )
                           (t :values)))
        (args-formals-orig args-formals)
        (values-formals-orig values-formals)
        (defer-form-decls nil))
    (when (stringp (car forms))       (setf docstring (pop forms)))
    (when (eq (caar forms) 'declare)  (setf form-decls (cdr (pop forms))))
    (let ((core-form
           `(returning-criterion-config-error
                (,(format nil "Error from criterion ~s body" name))
              ,@forms)))

      ;; Set the fp symbol; and wrap the core-form if we want its
      ;; values.
      (case values-list-type
        ((:values)
         (setf fp (gensym "values-form")
               core-form
               (let ((vs (gensym "values")))
                 `(let ((,vs (returning-test-error (eval ,fp))))
                    (returning-criterion-config-error
                        ((format nil
                             "Values under test ~a do not match lambda-list ~a"
                           ,vs ',values-formals-orig))
                      (destructuring-bind ,values-formals ,vs
                        ,@(when form-decls `((declare ,@form-decls)))
                        ,core-form))))))
        ((:form)
         (setf fp (car values-formals)
               defer-form-decls t))
        (otherwise
         (error "Unrecognized tag ~s for tested forms lambda list."
                values-list-type)))

      (when (eq args-list-type :values)
        (multiple-value-bind (form-deconstructor values-binders)
            (decompose-arg-values-lambda-list args-formals)
          (setf core-form `(let ,values-binders
                             ,@(when (and form-decls defer-form-decls)
                                 `((declare ,@form-decls)))
                             ,core-form)
                args-formals form-deconstructor)))

      (case args-list-type
        ((:values :forms)
         (setf core-form
           `(returning-criterion-config-error
                ((format nil
                     "Criterion arguments ~a do not match lambda-list ~a"
                   ,ap ',args-formals-orig))
              (destructuring-bind ,args-formals ,ap
                ,@(when (and form-decls defer-form-decls)
                    `((declare ,@form-decls)))
                ,core-form))))
        (otherwise
         (error "Unrecognized tag ~s for criteria arguments lambda list"
                args-list-type)))

      `(progn
         #+allegro (excl:record-source-file ',name :type :nst-criterion)
         (defmethod apply-criterion ((top (eql ',name)) ,ap ,fp)
           (declare (optimize (debug 3)))
           ,@(when docstring (list docstring))
           ,core-form)
         ,@(when docstring
             `((setf (documentation ',name :nst-criterion) ,docstring)))))))
(def-documentation (compiler-macro def-criterion)
    (:tags primary)
    (:intro (:latex "The \\texttt{def-criterion} macro defines a new criterion for use in NST tests.\index{def-criterion@\texttt{def-criterion}}"))
  (:callspec ((name criterion-lambda-list values-lambda-list)
                   &body
                   (:opt documentation)
                   (:seq FORM)))
  (:full (:latex "These criteria definitions are like generic function method
definitions with two sets of formal parameters:")
         (:itemize ()
          (:latex "The forms provided as the actual parameters of the criterion  itself.")
          (:latex "The values arising from the evaluation of the forms under test."))
         (:latex "The body of a \\texttt{def-criterion} should return a test result report contructed with the \\texttt{make-success-report}, etc.\\ functions.")
         (:seq
          (:latex "Examples:")
          (:code "(def-criterion (:true () (bool))
  (if bool
      (make-success-report)
      (make-failure-report :format \"Expected non-null, got: ~s\"
                    :args (list bool))))

(def-criterion (:eql (target) (actual))
  (if (eql (eval target) actual)
      (make-success-report)
      (make-failure-report :format \"Not eql to value of ~s\"
                    :args (list target))))"))))

;; #+allegro (excl::define-simple-parser def-criterion-unevaluated
;;              caadr :nst-criterion)
(defmacro def-criterion-unevaluated ((name args-formals forms-formal &key
                                           (ignore-forms nil))
                                     &body forms)
  (let ((ap (gensym "args"))
        (docstring nil)
        (form-decls nil))
    (when (stringp (car forms))       (setf docstring (pop forms)))
    (when (eq (caar forms) 'declare)  (setf form-decls (cdr (pop forms))))
    `(progn
       #+allegro (excl:record-source-file ',name :type :nst-criterion)
       (defmethod apply-criterion ((top (eql ',name)) ,ap ,forms-formal)
         (declare (optimize (debug 3))
                  ,@(when ignore-forms `((ignore ,forms-formal))))
         ,@(when docstring (list docstring))
         (returning-criterion-config-error
             ((format nil "Criterion arguments ~a do not match lambda-list ~a"
                ,ap ',args-formals))
           (destructuring-bind ,args-formals ,ap
             ,@(when form-decls `((declare ,@form-decls)))
             (returning-criterion-config-error
                 (,(format nil "Error from criterion ~s body" name))
               ,@forms))))
       ,@(when docstring
           `((setf (documentation ',name :nst-criterion) ,docstring))))))
(def-documentation (compiler-macro def-criterion-unevaluated)
    (:tags primary)
  (:intro "Define a new criterion for use in NST tests.")
  (:callspec ((name criterion-lambda-list forms-arg) &body
              (:opt documentation) (:seq form))))

#+allegro (excl::define-simple-parser def-values-criterion caadr :nst-criterion)
(defmacro def-values-criterion ((name args-formals forms-formals &key
                                      (declare nil decl-supp-p))
                                &body forms)
  (warn 'nst-soft-deprecation :old-name 'def-values-criterion
        :replacement 'def-criterion)
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
(def-documentation (compiler-macro def-values-criterion)
    (:tags primary)
    (:deprecated t)
    (:short "DEPRECATED: use def-criterion instead."))


#+allegro (excl::define-simple-parser def-form-criterion caadr :nst-criterion)
(defmacro def-form-criterion ((name args-formals form-formal) &rest forms)
  (warn
   "def-form-criterion is deprecated from 1.3.0, AND PROBABLY WILL NOT WORK.")
  (let ((ap (gensym "args")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,form-formal)
       (destructuring-bind ,args-formals ,ap
         (eval (progn ,@forms))))))
(def-documentation (compiler-macro def-form-criterion)
    (:tags primary)
    (:deprecated t)
    (:short "DEPRECATED: use def-criterion-unevaluated instead."))

(defmacro def-criterion-alias ((name . args-formals) docstring-or-form
                               &optional (form nil form-supp-p))
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
(def-documentation (compiler-macro def-criterion-alias)
    (:tags primary)
    (:intro "Define one criterion in terms of another.")
  (:callspec ((name &rest args) &body (:opt documentation) expansion)))

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
