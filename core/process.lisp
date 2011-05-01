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

(defun assert-via-simple-predicate (pred target tested default-message &key
                                    (format nil format-supp-p)
                                    (format-args nil format-args-supp-p)
                                    (fatal nil))
  (unless format-supp-p (setf format default-message))
  (unless format-args-supp-p (setf format-args (list target tested)))
  (with-simple-restart (nst-assertion
                        (apply #'format nil format format-args))
    (unless (funcall pred target tested)
      (error 'nst-assertion-failure
             :fatal fatal :args format-args :formatter format))))

(defvar *assert-eq-format-string* "~@<Expected eq to ~s, ~_got ~s~:>")
(defun assert-eq (target tested &rest keyargs &key &allow-other-keys)
  (apply #'assert-via-simple-predicate
         #'eq target tested *assert-eq-format-string* keyargs))

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
  (:properties (nst-manual processes-criteria))
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
