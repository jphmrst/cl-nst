;;; File criteria.lisp
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

;;; Built-in basic testing criteria.

(def-values-criterion (:pass () (&rest chk) :declare ((ignorable chk)))
  `(check-result))

(def-values-criterion (:fail (&rest args) (&rest chk) :declare ((ignorable chk)))
  `(emit-failure :format ,(car args) :args ,(cdr args)))

(def-values-criterion (:warn (&rest args) (&rest chk) :declare ((ignorable chk)))
  ;; `(declare (ignorable chk))
  `(emit-warning :format ,(car args) :args ,(cdr args)))

(def-values-criterion (:true () (bool))
  `(if bool
     (check-result)
     (emit-failure :format "Form not t: ~s" :args (list bool))))

(def-values-criterion (:eq (eq-form) (check-form))
  `(if (eq ,eq-form check-form)
     (check-result)
     (emit-failure :format "Not eq to value of ~s" :args '(,eq-form))))

(def-criterion-alias (:symbol name) `(:eq ',name))

(def-values-criterion (:eql (eql-form) (check-form))
  `(if (eql ,eql-form check-form)
     (check-result)
     (emit-failure :format "Not eql to value of ~s" :args '(,eql-form))))

(def-values-criterion (:equal (eql-form) (check-form))
  `(if (equal ,eql-form check-form)
     (check-result)
     (emit-failure :format "Not equal to value of ~s" :args '(,eql-form))))

(def-values-criterion (:equalp (eql-form) (check-form))
  `(if (equalp ,eql-form check-form)
     (check-result)
     (emit-failure :format "Not equalp to value of ~s" :args '(,eql-form))))

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(def-criterion-alias (:forms-eql)   `(:predicate eql))
(def-criterion-alias (:forms-equal) `(:predicate equal))
(def-criterion-alias (:value-list further) `(:apply list ,further))

(def-values-criterion (:predicate (pred) (&rest forms))
  `(if (apply #',pred forms)
     (check-result)
     (emit-failure :format "Predicate ~s fails" :args '(,pred))))

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))


(def-values-criterion (:dump-forms (blurb) (&rest forms))
  `(progn
     (format t "~%~a~%~{~s~%~}" ,blurb forms)
     (emit-failure :format "Arguments dumped" :args nil)))

(def-form-criterion (:info (string subcriterion) expr-list-form)
  (let ((subcheck (gensym "subcheck")))
    `(let ((,subcheck ,(continue-check subcriterion expr-list-form)))
       (push ,string (check-result-info ,subcheck))
       ,subcheck)))

(def-form-criterion (:err (&key (type 'condition type-supp-p)) expr-form)
  (let ((x (gensym "x")) (block (gensym "block")))
    `(block ,block
       (handler-bind ((,type #'(lambda (,x)
                                 (declare (ignorable ,x))
                                 (return-from ,block (check-result))))
                      ,@(when (and type-supp-p (not (eq type 'error)))
                          `((error
                             #'(lambda (,x)
                                 (unless *debug-on-error*
                                   (return-from ,block (emit-error ,x))))))))
         ,expr-form)
       (emit-failure :format "~@<No expected error:~{~_ ~s~}~:>"
                     :args '(,(cond
                               ((and (listp expr-form)
                                     (eq 'list (car expr-form)))
                                (cdr expr-form))
                               (t (list expr-form))))))))

(def-form-criterion (:perf (&key (ms nil ms-supp-p)
                                (sec nil sec-supp-p)
                                (min nil min-supp-p)) expr-form)
  (let ((core-form expr-form))
    (cond
     ((or sec-supp-p min-supp-p ms-supp-p)
      (when (or (and sec-supp-p min-supp-p) (and ms-supp-p min-supp-p)
                (and ms-supp-p sec-supp-p))
        (error "Multiple time metrics given"))
      (when sec-supp-p (setf ms (* 1000 sec)))
      (when min-supp-p (setf ms (* 60000 min)))
      (setf core-form
        `(let* ((start-time (get-internal-real-time))
                (*nst-stack* ,core-form)
                (elapsed-ms
                 (* ,(/ 1000
                        internal-time-units-per-second)
                    (- (get-internal-real-time)
                       start-time))))
           (if (> ,ms elapsed-ms)
             (check-result)
             (emit-failure
              :format "Execution time ~dms exceeded allowed time ~dms"
              :args '(elapsed-ms ,ms))))))
     (t
       (error
        ":perf check requires performance criteria specification")))
    core-form))

(def-form-criterion (:not (subcriterion) expr-list-form)
  (let ((subcheck (gensym)))
    `(let ((,subcheck ,(continue-check subcriterion expr-list-form)))
       (cond
        ((check-result-errors ,subcheck)
         ,subcheck)
        ((check-result-failures ,subcheck)
         (check-result :info (check-result-info ,subcheck)))
        (t
         (emit-failure :format "Expected failure from ~s"
                       :args '(,subcriterion)))))))


(def-form-criterion (:all (&rest args) expr-list-form)
  (let ((var (gensym "var")) (warnings (gensym "warnings"))
        (failures (gensym "failures")) (errors (gensym "errors"))
        (info (gensym "info"))
        (*nst-context-evaluable* t))
    (declare (special *nst-context-evaluable*))
    (labels ((test-next (args)
               (cond
                ((null args)
                 `(check-result :warnings ,warnings
                                     :failures ,failures
                                     :errors ,errors :info ,info))
                (t
                 (destructuring-bind (first-arg &rest other-args) args
                   (let ((subcheck (gensym "sub")))
                     `(let ((,subcheck
                             ,(continue-check first-arg var)))
                        (setf ,warnings
                          (nconc ,warnings
                                 (check-result-warnings ,subcheck))
                          ,failures
                          (nconc ,failures
                                 (check-result-failures ,subcheck))
                          ,errors
                          (nconc ,errors
                                 (check-result-errors ,subcheck))
                          ,info
                          (nconc ,errors
                                 (check-result-info ,subcheck)))
                        (cond
                          ((or ,failures ,errors)
                           (check-result :warnings ,warnings
                                              :failures ,failures
                                              :errors ,errors
                                              :info ,info))
                          (t ,(test-next other-args))))))))))
      `(let ((,var ,expr-list-form) ,warnings ,failures ,errors ,info)
        ,(test-next args)))))

(def-form-criterion (:any (&rest criteria) expr-list-form)
  (let ((new-stack (gensym "expr-list-form"))
        (block (gensym "block")) (info (gensym "info"))
        (result (gensym "result"))
        (rf (gensym "rf")) (re (gensym "re")) (ri (gensym "ri"))
        (*nst-context-evaluable* t))
    `(let ((,new-stack ,expr-list-form) (,info nil))
       (block ,block
         ,@(loop for criterion in criteria collect
                 `(let* ((,result ,(continue-check criterion new-stack))
                         (,rf (check-result-failures ,result))
                         (,re (check-result-errors ,result))
                         (,ri (check-result-info ,result)))
                    (cond
                      ((check-result-errors ,result)
                       (setf ,info (nconc ,info ,re ,rf ,ri)))
                      (,rf
                       (setf ,info (append ,info ,rf ,ri)))
                      (t
                       (return-from ,block
                         (check-result
                          :info (nconc ,info
                                       (check-result-info ,result))))))))
         (emit-failure :format "No disjuncts succeeded:~{~_ ~s~}"
                       :args '(,criteria)
                       :info ,info)))))

(def-form-criterion (:apply (transform criterion) forms)
  (continue-check criterion
                  `(multiple-value-call #'list (apply #',transform ,forms))))


(def-form-criterion (:check-err (criterion) forms)
  (let ((x (gensym "x"))
        (*error-checking* t))
    (declare (special *error-checking*))
    `(block ,x
       (handler-bind ((error #'(lambda (,x)
                                 (declare (ignorable ,x))
                                 (return-from ,x (check-result)))))
         ,(continue-check criterion forms))
       (emit-failure :format "~@<No expected error for check ~s on:~
                                 ~{~_ ~s~}~:>"
                     :args '(,criterion
                             ,(cond ((and (listp forms)
                                          (eq 'list (car forms)))
                                     (cdr forms))
                                    (t (list forms))))))))

(def-form-criterion (:progn (&rest forms-and-criterion) forms)
  (let ((progn-forms (butlast forms-and-criterion))
        (criterion (car (last forms-and-criterion))))
    `(progn
       ,@progn-forms
       ,(continue-check criterion forms))))

(def-form-criterion (:proj (indices criterion) forms)
  (let ((var (gensym))
        (*nst-context-evaluable* t))
    `(let ((,var ,forms))
       ,(continue-check criterion
                        `(list ,@(loop for idx in indices collect
                                       `(nth ,idx ,var)))))))

(def-form-criterion (:values (&rest args) forms)
  (continue-check `(:apply list (:seq ,@args)) forms))

(def-form-criterion (:each (criterion) forms)
  (let ((block (gensym)) (list (gensym "list")) (var (gensym "var"))
        (result (gensym "result"))
        (warnings (gensym "w")) (info (gensym "info"))
        (*nst-context-evaluable* t))
    `(block ,block
       (let ((,info nil) (,warnings nil))
         (destructuring-bind (,list) ,forms
           (loop for ,var in ,list do
             (let ((,result ,(continue-check criterion `(list ,var))))
               (cond
                ((or (check-result-errors ,result)
                     (check-result-failures ,result))
                 (setf (check-result-info ,result)
                   (append ,info (check-result-info ,result))
                   (check-result-warnings ,result)
                   (append ,warnings
                           (check-result-warnings ,result)))
                 (return-from ,block ,result))
                (t
                 (setf ,info (append ,info (check-result-info ,result))
                       ,warnings
                       (append ,warnings
                               (check-result-warnings ,result))))))))
         (check-result :info ,info :warnings ,warnings)))))



(def-form-criterion (:seq (&rest criteria) forms)
  (let ((block (gensym)) (list (gensym "list"))
        (result (gensym "result")) (warnings (gensym "w"))
        (info (gensym "info"))
        (*nst-context-evaluable* t))
    (declare (special *nst-context-evaluable*))
    `(block ,block
       (let ((,info nil) (,warnings nil))
         (destructuring-bind (,list) ,forms
           (unless (eql (length ,list) ,(length criteria))
             (return-from ,block
               (emit-failure :format "Expected list of length ~d"
                             :args '(,(length criteria)))))
           ,@(loop for criterion in criteria for idx from 0
                   collect
                   `(let ((,result
                           ,(continue-check criterion
                                            `(list (nth ,idx ,list)))))
                      (cond
                       ((or (check-result-errors ,result)
                            (check-result-failures ,result))
                        (setf (check-result-info ,result)
                          (append ,info (check-result-info ,result))
                          (check-result-warnings ,result)
                          (append ,warnings
                                  (check-result-warnings ,result)))
                        (return-from ,block ,result))
                       (t
                        (setf ,info
                              (append ,info (check-result-info ,result))
                              ,warnings
                              (append
                               ,warnings
                               (check-result-warnings ,result))))))))
         (check-result :info ,info :warnings ,warnings)))))

(def-form-criterion (:permute (criterion) forms)
  (let ((permute-block (gensym)) (list (gensym "list-"))
        (perms (gensym "perms-")) (x (gensym "x-"))
        (result (gensym "result")))
    `(block ,permute-block
       (destructuring-bind (,list) ,forms
         (let ((,perms (make-instance 'permuter :src ,list)))
           (loop while (has-next ,perms) do
             (let* ((,x (next-permutation ,perms))
                    (,result ,(continue-check criterion `(list ,x))))
               (cond
                 ((and (null (check-result-errors ,result))
                       (null (check-result-failures ,result)))
                  (return-from ,permute-block
                    (check-result)))))))))))


(def-form-criterion (:across (&rest criteria) forms)
  (let ((block (gensym)) (list (gensym "list"))
        (result (gensym "result")) (warnings (gensym "w"))
        (info (gensym "info")))
    `(block ,block
       (let ((,info nil) (,warnings nil))
         (destructuring-bind (,list) ,forms
           (unless (eql (length ,list) ,(length criteria))
             (return-from ,block
               (emit-failure :format "Expected list of length ~d"
                             :args '(,(length criteria)))))
           ,@(loop for criterion in criteria for idx from 0
                   collect
                   `(let ((,result
                           ,(continue-check criterion
                                            `(list (aref ,list ,idx)))))
                      (cond
                       ((or (check-result-errors ,result)
                            (check-result-failures ,result))
                        (setf (check-result-info ,result)
                          (append ,info (check-result-info ,result))
                          (check-result-warnings ,result)
                          (append ,warnings
                                  (check-result-warnings ,result)))
                        (return-from ,block ,result))
                       (t
                        (setf ,info
                              (append ,info (check-result-info ,result))
                              ,warnings
                              (append
                               ,warnings
                               (check-result-warnings ,result))))))))
         (check-result :info ,info :warnings ,warnings)))))


(def-form-criterion (:slots (&rest clauses) forms)
  (let ((block (gensym "oblock-")) (obj (gensym "o-"))
        (slot-criterion (make-hash-table :test 'eq))
        (result (gensym "result"))
        (warnings (gensym "w")) (info (gensym "info")))
    (loop for (slot criterion) in clauses do
      (setf (gethash slot slot-criterion) criterion))
    `(block ,block
       (let ((,warnings nil) (,info nil))
         (destructuring-bind (,obj) ,forms
           ,@(loop for slot being the hash-keys of slot-criterion
                   using (hash-value criterion)
                   collect
                   `(with-slots (,slot) ,obj
                      (let ((,result
                             ,(continue-check criterion `(list ,slot))))
                        (cond
                          ((or (check-result-errors ,result)
                               (check-result-failures ,result))
                           (setf (check-result-info ,result)
                                 (append ,info
                                         (check-result-info ,result))
                                 (check-result-warnings ,result)
                                 (append ,warnings
                                         (check-result-warnings
                                          ,result)))
                           (return-from ,block ,result))
                       (t
                        (setf ,info
                              (append ,info (check-result-info ,result))
                              ,warnings
                              (append
                               ,warnings
                               (check-result-warnings ,result))))))))
         (check-result :info ,info :warnings ,warnings))))))
