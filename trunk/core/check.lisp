;;; File check.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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
(in-package :sift.nst)

(defgeneric apply-criterion (top args form)
  (:documentation
   "Internal generic function whose methods are the translations of criteria."))

(defmacro returning-criterion-config-error ((msg) &body forms)
  "For use within criteria definitions only --- catch errors and process them
as errors in the \"glue\" among criteria and forms."
  `(handler-bind-interruptable
       ((error
         (named-function returning-criterion-config-error-handler
           (lambda (error)
             (unless *debug-on-error*
               (return-from apply-criterion
                 (make-error-report error
                                    :format (format nil "Criterion error: ~a"
                                              ,msg))))))))
     ,@forms))

(defmacro returning-test-error (&body forms)
  "For use within criteria definitions only --- catch errors and process them
as errors arising from within the ."
  `(handler-bind-interruptable
       ((error (named-function returning-test-error-handler
                 (lambda (e)
                   (unless *debug-on-error*
                     (return-from apply-criterion
                       (make-error-report e)))))))
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
                 ((listp item) (extract-parameters item))
                 ((and (symbolp item)
                       (eql (symbol-package item) (find-package :common-lisp)))
                  nil)
                 (t (list item)))))

(defun continue-check #|continue-check-actual|# (criterion form)
  (unless (listp criterion)
    (setf criterion (list criterion)))
  `(apply-criterion ',(car criterion) ',(cdr criterion) ',form))

(defun check-criterion-on-value (criterion val)
  "The =check-criterion-on-value= function can be called from within a criterion
body to verify that a value adheres to a criterion."
  (check-criterion-on-form criterion `(list ',val)))
(def-documentation (function check-criterion-on-value)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro (:latex "The \\texttt{check-criterion-on-value} function can be called from within a criterion body to verify that a value adheres to a criterion."))
    (:blurb "This function verifies that the value adheres to the criterion.")
    (:callspec (criterion value)))
(defun check-criterion-on-form (criterion form)
  "This function verifies that the values return by evaluating the form adheres
to the criterion."
  (unless (listp criterion)
    (setf criterion (list criterion)))
  (apply-criterion (car criterion) (cdr criterion) form))
(def-documentation (function check-criterion-on-form)
  (:tags criteria)
  (:properties (api-summary criteria))
    (:intro (:latex "The \\texttt{check-criterion-on-form} function verifies that an unevaluated form adheres to a criterion."))
    (:blurb
     "This function verifies that the values return by evaluating the form adheres to the criterion.")
    (:callspec (criterion form)))

(defun build-continue-check-expr (criterion form)
  (when (symbolp criterion) (setf criterion (list criterion)))
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
  "The =def-criterion= macro defines a new criterion for use in NST tests.
These criteria definitions are like generic function method definitions with two
sets of formal parameters: the forms provided as the actual parameters of the
criterion itself, and the values arising from the evaluation of the forms under
test.
#+begin_example
\(def-criterion (name criterion-lambda-list values-lambda-list)
  [ doc-string )
  form
  form
  ...
  form)
#+end_example

- name :: Name of the criterion.
- criterion-lambda-list :: Lambda list for the arguments to the criterion.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: =:values= for call-by-value, or =:forms for call-by-name (the default).  The list may include the keywords =&key=, =&optional=, =&body= and =&rest= but may not use =&whole= or =&environment=.  Apart from this restriction, in the former case the list may be any ordinary lambda list as for =defun=, and in the latter case the list may be any macro lambda list as for =defmacro=.
- values-lambda-list :: Lambda list for the forms under test.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: :values for call-by-value (the default), or :form for call-by-name.  In the former case, the list may include the keywords =&key=, =&optional=, =&body= and =&rest=, but not =&whole= or =&environment=; apart from that restriction, list may be any ordinary lambda list as for =defun=.  In the latter case, the remainder of the list must contain exactly one symbol, to which a form which would evaluate to the values under test will be bound.
                        If the criterion ignores the values, then instead of a lambda list, this argument may be the symbol =:ignore=.  On many platforms, listing a dummy parameter which is then =declare=d =ignore= or =ignorable= will produce a style warning: the body of a =def-criterion= should not be assumed to correspond directly to the body of a =defmethod=; in general there will be surrounding =destructuring-bind=s.
- documentation :: An optional documentation string for the criterion.
- form :: The body of the criterion definition should return a test result report contructed with the =make-success-report=, etc. functions.
Examples:
#+begin_example
\(def-criterion (:true () (bool))
  \(if bool
      \(make-success-report)
      \(make-failure-report :format \"Expected non-null, got: ~s\"
                    :args \(list bool))))

\(def-criterion (:eql \(target) \(actual))
  \(if (eql \(eval target) actual)
      \(make-success-report)
      \(make-failure-report :format \"Not eql to value of ~s\"
                    :args \(list target))))
#+end_example
"

  (let* ((fp)
         (ap (gensym "args"))
         (method-declares nil)
         (docstring nil)
         (form-decls nil)
         (args-list-type (cond
                          ((and (consp args-formals)
                                (symbolp (car args-formals))
                                (eq (symbol-package (car args-formals))
                                    (find-package :keyword)))
                           (pop args-formals))
                          (t :forms)))
         (values-list-type (cond
                            ((eq values-formals :ignore)
                             (let ((genform (gensym)))
                               (setf values-formals (list genform))
                               (push `(ignore ,genform) method-declares))
                             :form)
                            ((and (consp values-formals)
                                  (symbolp (car values-formals))
                                  (eq (symbol-package (car values-formals))
                                      (find-package :keyword)))
                             (pop values-formals))
                            (t
                             :values)))
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
           (declare (optimize (debug 3)) ,@method-declares)
           ,@(when docstring (list docstring))
           ,core-form)
         ,@(when docstring
             `((setf (documentation ',name :nst-criterion) ,docstring)))))))
(def-documentation (macro def-criterion)
  (:tags primary)
  (:properties (api-summary primary))
  (:intro (:latex "The \\texttt{def-criterion} macro defines a new criterion for use in NST tests.  These criteria definitions are like generic function method definitions with two sets of formal parameters: the forms provided as the actual parameters of the criterion  itself, and the values arising from the evaluation of the forms under test.\\index{def-criterion@\texttt{def-criterion}}"))
  (:callspec ((name criterion-lambda-list values-lambda-list)
                   &body
                   (:opt documentation)
              (:seq form)))
  (:params (name "Name of the criterion.")
           (criterion-lambda-list (:latex "Lambda list for the arguments to the criterion.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: \\texttt{:values} for call-by-value, or \\texttt{:forms} for call-by-name (the default).  The list may include the keywords \\texttt{\\&key}, \\texttt{\\&optional}, \\texttt{\\&body} and \\texttt{\\&rest} but may not use \\texttt{\\&whole} or \\texttt{\\&environment}.  Apart from this restriction, in the former case the list may be any ordinary lambda list as for \\texttt{defun}, and in the latter case the list may be any macro lambda list as for \\texttt{defmacro}."))
           (values-lambda-list
            (:paragraphs
             (:latex "Lambda list for the forms under test.  Optionally, the first element of the list is a symbol specifying the parameter-passing semantics for the criterion arguments: \\texttt{:values} for call-by-value (the default), or \\texttt{:form} for call-by-name.  In the former case, the list may include the keywords \\texttt{\\&key}, \\texttt{\\&optional}, \\texttt{\\&body} and \\texttt{\\&rest}, but not \\texttt{\\&whole} or \\texttt{\\&environment}; apart from that restriction, list may be any ordinary lambda list as for \\texttt{defun}.  In the latter case, the remainder of the list must contain exactly one symbol, to which a form which would evaluate to the values under test will be bound.")
             (:latex "If the criterion ignores the values, then instead of a lambda list, this argument may be the symbol \\texttt{:ignore}.  On many platforms, listing a dummy parameter which is then \\texttt{declare}d \\texttt{ignore} or \\texttt{ignorable} will produce a style warning: the body of a \\texttt{def-criterion} should not be assumed to correspond directly to the body of a \\texttt{defmethod}; in general there will be surrounding \\texttt{destructuring-bind}s.")))
           (documentation "An optional documentation string for the criterion.")
           (form (:latex "The body of the criterion definition should return a test result report contructed with the \\texttt{make-success-report}, etc.\\ functions.")))
  (:details (:seq
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

(defmacro def-criterion-unevaluated ((name args-formals forms-formal &key
                                           (ignore-forms nil))
                                     &body forms)
  "The =def-criterion-unevaluated= macro is deprecated as of NST 2.1.2.  It was
consolidated into the =def-criterion= macro.

Replace:
#+begin_example
(def-criterion-unevaluated name (pattern ... pattern) name
  BODY)
#+end_example
with:
#+begin_example
(def-criterion name (:forms pattern ... pattern)
                    (:form name)
                    BODY)
#+end_example
"
  (warn 'nst-soft-deprecation :old-name 'def-criterion-unevaluated
        :replacement 'def-criterion)
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
(def-documentation (macro def-criterion-unevaluated)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:intro (:latex "The \\texttt{def-criterion-unevaluated} macro is deprecated as of NST 2.1.2.  It was consolidated into the \\texttt{def-criterion} macro."))
  (:details (:seq (:latex " Replace:")
               (:code "(def-criterion-unevaluated name (pattern ... pattern) name
  BODY)")
               (:plain "with:")
               (:code "(def-criterion name (:forms pattern ... pattern)
                    (:form name)
  BODY)")))
;;;  (:intro (:latex "\\index{def-criterion-unevaluated@\\texttt{def-criterion-unevaluated}}As under \\texttt{def-criterion}, the body of these criteria
;;;definitions receive the forms provided as the actual parameters of the
;;;criterion itself, and should return a test result report.  However,
;;;these criteria receive the unevaluated forms under test, deciding when
;;;and whether to evaluate them."))
;;;  (:callspec ((name criterion-lambda-list forms-arg) &body
;;;              (:opt documentation) (:seq form)))
;;;  (:details (:seq (:latex "Example:")
;;;               (:code "  (def-form-criterion (:apply (transform criterion) forms)
;;;    (continue-check criterion
;;;                  `(multiple-value-list (apply #',transform ,forms))))
;;;
;;;  (def-form-criterion (:not (subcriterion) exprs-form)
;;;    (let ((subcheck (gensym)))
;;;      `(let ((,subcheck ,(continue-check subcriterion exprs-form)))
;;;         (cond
;;;          ((check-result-errors ,subcheck)
;;;           ,subcheck)
;;;          ((check-result-failures ,subcheck)
;;;           (check-result :info (check-result-info ,subcheck)))
;;;          (t
;;;           (make-failure-report :format \"Expected failure from ~s\"
;;;                         :args '(,subcriterion)))))))")))
  )

#+allegro (excl::define-simple-parser def-values-criterion caadr :nst-criterion)
(defmacro def-values-criterion ((name args-formals forms-formals &key
                                      (declare nil decl-supp-p))
                                &body forms)
  "The =def-values-criterion= macro was deprecated as of NST 1.3.0. For new
criteria, use =def-criterion= instead.  In the short term, code using
=def-values-criterion= should continue to work as before."
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
(def-documentation (macro def-values-criterion)
  (:tags deprecated)
  (:properties (api-summary deprecated))
    (:deprecated t)
    (:blurb (:latex "The \\texttt{def-values-criterion} macro was deprecated as of NST 1.3.0. For new criteria, use \\texttt{def-criterion} instead.  In the short term, code using \\texttt{def-values-criterion} should continue to work as before.")))

#+allegro (excl::define-simple-parser def-form-criterion caadr :nst-criterion)
(defmacro def-form-criterion ((name args-formals form-formal) &rest forms)
  "The =def-form-criterion= macro was deprecated as of NST 1.3.0. /Code using
=def-form-criterion= in any but the simplest ways is very likely to fail./ Use
=def-criterion= instead."
  (warn
   "def-form-criterion is deprecated from 1.3.0, AND PROBABLY WILL NOT WORK.")
  (let ((ap (gensym "args")))
    `(defmethod apply-criterion ((top (eql ',name)) ,ap ,form-formal)
       (destructuring-bind ,args-formals ,ap
         (eval (progn ,@forms))))))
(def-documentation (macro def-form-criterion)
  (:tags deprecated)
  (:properties (api-summary deprecated))
    (:deprecated t)
    (:blurb (:latex "The \\texttt{def-form-criterion} macro was deprecated as of NST 1.3.0. \\emph{Code using \\texttt{def-form-criterion} in any but the simplest ways is very likely to fail.} Use \\texttt{def-criterion} instead.")))

(defmacro def-criterion-alias ((name . args-formals) docstring-or-form
                               &optional (form nil form-supp-p))
  "The simplest mechanism for defining a new criterion involves simply
defining one criterion to rewrite as another using =def-criterion-alias=.
#+begin_example
(def-criterion-alias (name (:seq arg))
  [ doc-string ]
  expansion)
#+end_example
The body of the expansion should be a Lisp form which, when evaluated, returns
an S-expression quoting the new criterion which the rewrite should produce.  The
=arg= are passed as for Lisp macros: they are not evaluated and are most
typically comma-inserted into a backquoted result.  For example:
#+begin_example
(def-criterion-alias (:forms-eq) `(:predicate eq))
(def-criterion-alias (:symbol name) `(:eq ',name))
#+end_example"
  (unless form-supp-p (setf form docstring-or-form docstring-or-form nil))
  (let* ((vsf (gensym "form"))
         (redef `(def-criterion (,name (:forms ,@args-formals) (:form ,vsf))
                   ,@(when docstring-or-form (list docstring-or-form))
                   (check-criterion-on-form ,form ,vsf))))
    (cond
     (docstring-or-form `(progn
                           ,redef
                           ,@(when docstring-or-form
                               `((setf (documentation ',name :nst-criterion)
                                   ,docstring-or-form)))))
     (t redef))))
(def-documentation (macro def-criterion-alias)
  (:tags primary)
  (:properties (nst-manual criterion-alias) (api-summary primary))
    (:intro (:latex "The simplest mechanism for defining a new criterion involves simply
defining one criterion to rewrite as another using
\\texttt{def-criterion-alias}:\\index{def-criterion-alias@\\texttt{def-criterion-alias}}"))
    (:callspec ((name (:seq arg)) &body (:opt documentation) expansion))
    (:details (:latex "The body of the expansion should be a Lisp form which, when evaluated, returns an S-expression quoting the new criterion which the rewrite should produce.  The \\texttt{arg}s are passed as for Lisp macros: they are not evaluated and are most typically comma-inserted into a backquoted result.  For example:")
           (:code "  (def-criterion-alias (:forms-eq) `(:predicate eq))
  (def-criterion-alias (:symbol name) `(:eq ',name))")))

#+allegro (excl::define-simple-parser def-values-check caadr :nst-criterion)
(defmacro def-value-check (&rest args)
  "Deprecated: use def-criterion instead"
  (warn 'nst-soft-deprecation
        :old-name 'def-value-check :replacement 'def-criterion)
  `(def-values-criterion ,@args))
#+allegro (def-documentation (macro def-values-check)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb "Deprecated: use def-criterion instead"))

#+allegro (excl::define-simple-parser def-control-check caadr :nst-criterion)
(defmacro def-control-check (&rest args)
  "Deprecated: use def-criterion instead"
  (warn 'nst-soft-deprecation
        :old-name 'def-control-check :replacement 'def-criterion)
  `(def-form-criterion ,@args))
#+allegro (def-documentation (macro def-control-check)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb "Deprecated: use def-criterion instead"))

#+allegro (excl::define-simple-parser def-check-alias caadr :nst-criterion)
(defmacro def-check-alias (&rest args)
  "Deprecated: use def-criterion-alias instead"
  (warn 'nst-soft-deprecation
        :old-name 'def-check-alias :replacement 'def-criterion-alias)
  `(def-criterion-alias ,@args))
#+allegro (def-documentation (macro def-check-alias)
  (:tags deprecated)
  (:properties (api-summary deprecated))
  (:deprecated t)
  (:blurb "Deprecated: use def-criterion-alias instead"))
