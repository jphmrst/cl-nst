;;; File process.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
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

;;; Documentation generator for process-testing predicates
(defdoc:def-target-type process-predicate (:symbol-definition-nocheck t))

(define-condition nst-assertion-failure (condition)
    ((fatal :initarg :fatal :reader assertion-failure-fatal)
     (formatter :initarg :formatter :reader assertion-failure-formatter)
     (args :initarg :args :reader assertion-failure-args)))

(define-condition nst-assertion-result-check (condition)
    ((fatal :initarg :fatal :reader assertion-result-fatal)
     (result :initarg :result :reader assertion-result-result)
     (formatter :initarg :formatter :reader assertion-result-formatter)
     (args :initarg :args :reader assertion-result-args)))

(defun assert-criterion-fn (criterion-expr values-form
                            &key formatter format-args fatal)
  (with-simple-restart (nst-assertion
                        (apply #'format nil formatter format-args))
    (unless (listp criterion-expr)
      (setf criterion-expr (list criterion-expr)))
    (let ((result (apply-criterion (car criterion-expr) (cdr criterion-expr)
                                   values-form)))
      ;; FILL IN
      (error 'nst-assertion-result-check :fatal fatal :result result
             :formatter formatter :args format-args))))

(defmacro assert-criterion (key-args criterion-expr &rest value-exprs)
  `(assert-criterion-fn ',criterion-expr (list ,@value-exprs)
                        ,@key-args))
(defdoc:def-documentation (macro assert-criterion)
  (:properties (nst-manual process-predicate))
  (:intro (:seq "Macro " (:lisp macro assert-criterion)
                (:latex "\\fbox{FILL IN}"))))

(defmacro def-unary-predicate-assert (assert-fn predicate default-message &key
                                      (message-defvar nil defvar-supp-p)
                                      (doc-state-flag t)
                                      (pred-name predicate) &allow-other-keys)
  (let* ((tested (gensym))
         (keyargs (gensym))
         (format-supp-p (gensym))
         (format-args-supp-p (gensym))
         (the-defuns
          `((defun ,assert-fn (,tested &rest ,keyargs &key
                               (format nil ,format-supp-p)
                               (format-args nil ,format-args-supp-p)
                               (fatal nil) &allow-other-keys)
              (unless ,format-supp-p (setf format ,default-message))
              (unless ,format-args-supp-p (setf format-args (list ,tested)))
              (with-simple-restart (nst-assertion
                                    (apply #'format nil format format-args))
                (unless (funcall #',predicate ,tested)
                  (error 'nst-assertion-failure
                         :fatal fatal :args format-args :formatter format))))
            (defdoc:def-documentation (function ,assert-fn)
              (:properties (nst-manual process-predicate))
              (:callspec (TESTED-VALUE))
              (:intro (:seq "The " (:lisp function ,assert-fn)
                            " function is a unary predicate for use within the forms evaluated for an "
                            (:lisp criterion :eval)
                            " criterion.  It succeeds whenever the "
                            (:lisp function ,pred-name)
                            " function returns "
                            ,(if doc-state-flag "non-nil." "nil.")))))))
    (cond
      (defvar-supp-p
          `(progn (defvar ,message-defvar ,default-message)
                  ,@the-defuns))
      (t `(progn ,@the-defuns)))))
(defdoc:def-documentation (macro def-unary-predicate-assert)
  (:properties (nst-manual process-pred-maker))
  (:intro (:seq "Macro " (:lisp macro def-unary-predicate-assert)
                (:latex "\\fbox{FILL IN}"))))

(defmacro def-unary-negated-predicate-assert (assert-fn predicate
                                              default-message
                                              &rest keyargs &key message-defvar)
  (declare (ignore message-defvar))
  `(def-unary-predicate-assert ,assert-fn
       (lambda (x) (not (funcall #',predicate x)))
     ,default-message :doc-state-flag nil :pred-name ,predicate ,@keyargs))
(defdoc:def-documentation (macro def-unary-negated-predicate-assert)
  (:properties (nst-manual process-pred-maker))
  (:intro (:seq "Macro " (:lisp macro def-unary-negated-predicate-assert)
                (:latex "\\fbox{FILL IN}"))))

(defmacro def-binary-predicate-assert (assert-fn predicate default-message &key
                                       (message-defvar nil defvar-supp-p)
                                       (doc-state-flag t) (pred-name predicate))
  (let* ((target (gensym))
         (tested (gensym))
         (keyargs (gensym))
         (format-supp-p (gensym))
         (format-args-supp-p (gensym))
         (the-defuns
          `((defun ,assert-fn (,target ,tested &rest ,keyargs &key
                               (format nil ,format-supp-p)
                               (format-args nil ,format-args-supp-p)
                               (fatal nil) &allow-other-keys)
              (unless ,format-supp-p (setf format ,default-message))
              (unless ,format-args-supp-p (setf format-args
                                                (list ,target ,tested)))
              (with-simple-restart (nst-assertion
                                    (apply #'format nil format format-args))
                (unless (funcall #',predicate ,target ,tested)
                  (error 'nst-assertion-failure
                         :fatal fatal :args format-args :formatter format))))
            (defdoc:def-documentation (function ,assert-fn)
              (:properties (nst-manual process-predicate))
              (:callspec (EXPECTED-VALUE TESTED-VALUE))
              (:intro (:seq "The " (:lisp function ,assert-fn)
                            " function is a binary predicate for use within the forms evaluated for an "
                            (:lisp criterion :eval)
                            " criterion.  It compares the expected and tested values using "
                            (:lisp function ,pred-name)
                            ", and succeeds whenever that call returns "
                            ,(if doc-state-flag "non-nil." "nil.")))))))
    (cond
      (defvar-supp-p
          `(progn (defvar ,message-defvar ,default-message)
                  ,@the-defuns))
      (t `(progn ,@the-defuns)))))
(defdoc:def-documentation (macro def-binary-predicate-assert)
  (:properties (nst-manual process-pred-maker))
  (:intro (:seq "Macro " (:lisp macro def-binary-predicate-assert)
                (:latex "\\fbox{FILL IN}"))))

(defmacro def-binary-negated-predicate-assert (assert-fn predicate
                                               default-message &rest keyargs
                                               &key message-defvar)
  (declare (ignore message-defvar))
  `(def-binary-predicate-assert ,assert-fn
       (lambda (x y) (not (funcall #',predicate x y)))
     ,default-message :doc-state-flag nil :pred-name ,predicate ,@keyargs))
(defdoc:def-documentation (macro def-binary-negated-predicate-assert)
  (:properties (nst-manual process-pred-maker))
  (:intro (:seq "Macro " (:lisp macro def-binary-negated-predicate-assert)
                (:latex "\\fbox{FILL IN}"))))

(def-unary-predicate-assert assert-null null  "~@<Expected null, ~_got ~s~:>"
                            :message-defvar *assert-null-format-string*)
(def-unary-predicate-assert assert-zero zerop "~@<Expected zero, ~_got ~s~:>"
                            :message-defvar *assert-zero-format-string*)
(def-unary-negated-predicate-assert assert-non-nil null
  "~@<Expected non-nil, ~_got ~s~:>"
  :message-defvar *assert-nonnil-format-string*)

(def-binary-predicate-assert assert-eq eq
  "~@<Expected eq to ~s, ~_got ~s~:>"
  :message-defvar *assert-eq-format-string*)

(def-binary-predicate-assert assert-eql eql
  "~@<Expected eql to ~s, ~_got ~s~:>"
  :message-defvar *assert-eql-format-string*)

(def-binary-predicate-assert assert-equal equal
  "~@<Expected equal to ~s, ~_got ~s~:>"
  :message-defvar *assert-equal-format-string*)

(def-binary-predicate-assert assert-equalp equalp
  "~@<Expected equalp to ~s, ~_got ~s~:>"
  :message-defvar *assert-equalp-format-string*)

(def-binary-negated-predicate-assert assert-not-eq eq
  "~@<Expected non-eq to ~s, ~_got ~s~:>"
  :message-defvar *assert-not-eq-format-string*)

(def-binary-negated-predicate-assert assert-not-eql eql
  "~@<Expected non-eql to ~s, ~_got ~s~:>"
  :message-defvar *assert-not-eql-format-string*)

(def-binary-negated-predicate-assert assert-not-equal equal
  "~@<Expected non-equal to ~s, ~_got ~s~:>"
  :message-defvar *assert-not-equal-format-string*)

(def-binary-negated-predicate-assert assert-not-equalp equalp
  "~@<Expected non-equalp to ~s, ~_got ~s~:>"
  :message-defvar *assert-not-equalp-format-string*)

(def-criterion (:eval (:forms &key (check-warnings t) (muffle-warnings t)
                              (attempt-continue t) force-continue)
                      (:form forms-list))
  ;; Should have a better way of working out whether we have a list of
  ;; forms top-level here.
  (case (car forms-list)
    ((multiple-value-list list)
     (pop forms-list)))

  (let ((result (make-success-report)))
    (block process
      (flet ((nst-assertion-restart (condition)
               (cond
                 ((or force-continue
                      (and attempt-continue
                           (not (assertion-failure-fatal condition))))
                  (let ((restart (find-restart 'nst-assertion condition)))
                    (cond
                      (restart (invoke-restart restart))
                      (t (return-from process)))))
                 (t (return-from process)))))
        (handler-bind ((nst-assertion-failure
                        #'(lambda (e)
                            (add-failure result
                              :format (assertion-failure-formatter e)
                              :args (assertion-failure-args e))
                            (nst-assertion-restart e)))
                       (nst-assertion-result-check
                        #'(lambda (e)
                            (add-failure result
                              :format (assertion-failure-formatter e)
                              :args (assertion-failure-args e))
                            (nst-assertion-restart e)))
                       (error #'(lambda (e)
                                  (add-error result
                                    :format "~w" :args (list e))
                                  (return-from process)))
                       (warning #'(lambda (w)
                                    (when check-warnings
                                      (add-warning result w))
                                    (when muffle-warnings
                                      (muffle-warning)))))
          (eval `(progn ,@forms-list)))))
    (calibrate-check-result result)))
(defdoc:def-documentation (criterion :eval)
  (:properties (nst-manual process))
  (:callspec (&key (check-warnings FLAG) (muffle-warnings FLAG)
                   (attempt-continue FLAG) (force-continue FLAG)))
  (:intro (:seq "The " (:lisp criterion :eval) " criterion executes its forms, expecting calls to various assertion functions to check intermediate states of an arbitrarily-long process."))
  (:params (check-warnings "If non-nil, will add warnings thrown when evaluating the forms under test as NST warnings.  The default is " (:inline "t") ".")
           (muffle-warnings "If non-nil, will muffle warnings thrown when evaluating the forms under test, so that they are reported only as NST result warnings and if the " (:inline ":check-warnings") " flag is set.  The default is " (:inline "t") ".")
           (attempt-continue "If non-nil, will continue evaluation after failed assertions, so long as the failure is not deemed " (:inline "fatal") ". The default is " (:inline "t") ".")
           (force-continue "If non-nil, will continue evaluation after failed assertions even if the failure is not deemed " (:inline "fatal") ". The default is " (:inline "nil") ".")))

(def-criterion (:process (:forms &rest forms) :ignore)
    (let ((result (make-success-report)))
      (block process
        (loop for form in forms do
              (unless (and (consp form) (symbolp (car form)))
                (add-error result
                  :format "Expected (STEP ...), got ~s" :args (list form)))
              (case (car form)
                ((:eval) (block eval-forms
                           (handler-bind ((error
                                           #'(lambda (e)
                                               (add-thrown-error result e)
                                               (return-from eval-forms))))
                             (eval `(progn ,@(cdr form))))))
                ((:check)
                 (setf result
                   (check-result-union result
                                       (check-criterion-on-form
                                        `(:all ,@(cdr form)) nil))))
                ((:failcheck) (when (or (check-result-failures result)
                                        (check-result-errors result))
                                (return-from process)))
                ((:errcheck)  (when (check-result-errors result)
                                (return-from process)))
                (otherwise
                 (setf result
                   (check-result-union result
                                       (check-criterion-on-form form nil)))))))
      (calibrate-check-result result)))
(defdoc:def-documentation (criterion :process)
  (:properties (nst-manual process-dep))
  (:callspec ((:seq form)))
  (:intro (:seq "The " (:lisp criterion :process) " criterion allows simple interleaving of Lisp function calls and NST checks, to allow checking of intermediate states of an arbitrarily-long process."))
  (:details (:latex "This criterion takes as its body a list of forms.  The first element of each form should be a symbol:")
            (:itemize ()
              (:latex "\\texttt{:eval} --- Heads a list of forms which should be evaluated.")
              (:latex "\\texttt{:check} --- Heads a list of criteria which should be checked.")
              (:latex "\\texttt{:failcheck} --- If checks to this point have generated any errors or failures, then the \\texttt{process} criterion is aborted.")
              (:latex "\\texttt{:errcheck} --- If checks to this point have generated any errors (but not failures), then the \\texttt{process} criterion is aborted."))
            (:latex "The \\texttt{:process} criterion takes no value arguments in a \\texttt{def-test}.")
            (:seq
             (:plain "Example:")
             (:code
              "(def-test process-1
    (:process (:eval (setf zzz 0))
              (:check (:true-form (eql zzz 0)))
              (:eval (incf zzz))
              (:check (:true-form (eql zzz 1)))
              (:eval (incf zzz))
              (:check (:true-form (eql zzz 2)))))")))
  )
