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

(def-criterion-unevaluated (:pass () chk)
  (declare (ignorable chk))
  (check-result))

(def-criterion-unevaluated (:fail (&rest args) chk)
  (declare (ignorable chk))
  (emit-failure :format (car args) :args (cdr args)))

(def-criterion-unevaluated (:warn (&rest args) chk)
  (declare (ignorable chk))
  (emit-warning :format (car args) :args (cdr args)))

(def-criterion (:true () (bool))
  (if bool
      (check-result)
      (emit-failure :format "Expected non-null form, got: ~s"
                    :args (list bool))))

(def-criterion (:eq (eq-form) (result))
  (if (eq (eval eq-form) result)
      (check-result)
      (emit-failure :format "Not eq to value of ~s:~_Value ~s"
                    :args `('eq-form ,result))))

(def-criterion-alias (:symbol name) `(:eq ',name))

(def-criterion (:eql (eql-form) (check-form))
  (if (eql (eval eql-form) check-form)
      (check-result)
      (emit-failure :format "Not eql to value of ~s" :args (list eql-form))))

(def-criterion (:equal (eql-form) (check-form))
  (if (equal (eval eql-form) check-form)
     (check-result)
     (emit-failure :format "Not equal to value of ~s" :args (list eql-form))))

(def-criterion (:equalp (eql-form) (check-form))
  (if (equalp (eval eql-form) check-form)
      (check-result)
      (emit-failure :format "Not equalp to value of ~s" :args (list eql-form))))

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(def-criterion-alias (:forms-eql)   `(:predicate eql))
(def-criterion-alias (:forms-equal) `(:predicate equal))
(def-criterion-alias (:value-list further) `(:apply list ,further))

(def-criterion (:predicate (pred) (&rest vals))
  (if (apply pred vals)
      (check-result)
      (emit-failure :format "Predicate ~s fails for ~s"
                    :args (list pred vals))))

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))


(def-criterion (:dump-forms (blurb) (&rest forms))
  (format t "~%~a~%~{~s~%~}" blurb forms)
  (emit-failure :format "Arguments dumped" :args nil))

(def-criterion-unevaluated (:info (string subcriterion) expr-list-form)
  (let ((subcheck (check-subcriterion-on-form subcriterion expr-list-form)))
    (push string (check-result-info subcheck))
    subcheck))

(def-criterion-unevaluated (:err (&key (type 'condition)) expr-form)
  (block err-criterion
    (format-at-verbosity 4 "    Setting up ~s handler for :err~%" type)
    (handler-bind
        ((condition #'(lambda (e)
                        (cond
                         ((typep e type)
                          (format-at-verbosity 4
                              "Caught ~s as expected by :err criterion~%" e)
                          (return-from err-criterion (check-result)))
                         ((typep e 'error)
                          (format-at-verbosity 4
                              "Caught ~s but :err expected ~s~%" e type)
                          (unless *debug-on-error*
                            (return-from err-criterion (emit-error e))))))))
      (eval expr-form))

    (emit-failure :format "~@<No expected error:~{~_ ~s~}~:>"
                  :args `(,(cond
                            ((and (listp expr-form)
                                  (eq 'list (car expr-form)))
                             (cdr expr-form))
                            (t (list expr-form)))))))

(def-criterion-unevaluated (:perf (&key (ms nil ms-supp-p)
                                        (sec nil sec-supp-p)
                                        (min nil min-supp-p)) expr-form)
  (cond
   ((or sec-supp-p min-supp-p ms-supp-p)
    (when (or (and sec-supp-p min-supp-p) (and ms-supp-p min-supp-p)
              (and ms-supp-p sec-supp-p))
      (error "Multiple time metrics given"))
    (when sec-supp-p (setf ms (* 1000 sec)))
    (when min-supp-p (setf ms (* 60000 min)))
    (let* ((start-time (get-internal-real-time))
           (eval-result (eval expr-form))
           (end-time (get-internal-real-time))
           (elapsed-ms
            (* (/ 1000 internal-time-units-per-second)
               (- end-time start-time))))
      (declare (ignore eval-result))
      (if (> ms elapsed-ms)
        (check-result)
        (emit-failure :format "Execution time ~dms exceeded allowed time ~dms"
                      :args (list elapsed-ms ms)))))
   (t
    (error ":perf check requires performance criteria specification"))))

(def-criterion-unevaluated (:not (subcriterion) expr-list-form)
  (let ((subcheck (check-subcriterion-on-form subcriterion expr-list-form)))
    (cond
     ((check-result-errors subcheck)   subcheck)
     ((check-result-failures subcheck) (check-result
                                        :info (check-result-info subcheck)))
     (t (emit-failure :format "Expected failure from ~s"
                      :args (list subcriterion))))))


(def-criterion-unevaluated (:all (&rest subcriteria) expr-list-form)
  (let ((*nst-context-evaluable* t))
    (declare (special *nst-context-evaluable*))
    (let (warnings failures errors info)
      (loop for subcriterion in subcriteria do
        (let ((subresult (check-subcriterion-on-form subcriterion expr-list-form)))
          (setf warnings (nconc warnings (check-result-warnings subresult))
                failures (nconc failures (check-result-failures subresult))
                errors (nconc errors (check-result-errors subresult))
                info (nconc errors (check-result-info subresult)))))
      (check-result :warnings warnings :failures failures
                    :errors errors :info info))))

(def-criterion-unevaluated (:any (&rest criteria) expr-list-form)
  (let ((*nst-context-evaluable* t) (info nil))
    (block any-criterion
      (loop for criterion in criteria do
        (let* ((result (check-subcriterion-on-form criterion expr-list-form))
               (rf (check-result-failures result))
               (re (check-result-errors result))
               (ri (check-result-info result)))
          (when re (setf info (nconc info re)))
          (when rf (setf info (nconc info rf)))
          (when ri (setf info (nconc info ri)))
          (unless (or re rf)
            (return-from any-criterion
              (check-result :info (nconc info (check-result-info result))))))))
    (emit-failure :format "No disjuncts succeeded:~{~_ ~s~}"
                  :args (list criteria) :info info)))

(def-criterion-unevaluated (:apply (transform criterion) exprs-form)
  (check-subcriterion-on-form criterion
    `(multiple-value-call #'list (apply #',transform ,exprs-form))))


(def-criterion-unevaluated (:check-err (criterion) forms)
  (block check-err-block
    (handler-bind ((error #'(lambda (e)
                              (format-at-verbosity 4
                                  "Caught ~s as expected by :check-err~%" e)
                              (return-from check-err-block (check-result)))))
      (check-subcriterion-on-form criterion forms))
    (emit-failure :format "~@<No expected error for check ~s on:~
                                 ~{~_ ~s~}~:>"
                  :args (list criterion
                              (cond ((and (listp forms)
                                          (eq 'list (car forms)))
                                     (cdr forms))
                                    (t (list forms)))))))

(def-criterion-unevaluated (:progn (&rest forms-and-criterion) forms)
  (let ((progn-forms (butlast forms-and-criterion))
        (criterion (car (last forms-and-criterion))))
    (loop for form in progn-forms do (eval form))
    (check-subcriterion-on-form criterion forms)))

(def-criterion (:proj (indices criterion) (&rest values))
  (check-subcriterion-on-form criterion
    `(list ,@(loop for idx in indices collect `',(nth idx values)))))

(def-criterion-unevaluated (:values (&rest args) form)
    (check-subcriterion-on-form `(:seq ,@args) `(list ,form)))

(def-criterion (:each (criterion) (l))
  (block each
    (let ((info nil) (warnings nil))
      (loop for value in l do
        (let ((result (check-subcriterion-on-form criterion `(list ',value))))
          (cond
            ((or (check-result-errors result)
                 (check-result-failures result))
             (setf (check-result-info result)
                   (append info (check-result-info result))
                   (check-result-warnings result)
                   (append warnings (check-result-warnings result)))
             (return-from each result))
            (t
             (setf info (append info (check-result-info result))
                   warnings (append warnings
                                (check-result-warnings result)))))))
      (check-result :info info :warnings warnings))))


(def-criterion (:seq (&rest criteria) (l))
  (block seq
    (let ((info nil) (warnings nil))
      (unless (eql (length l) (length criteria))
        (return-from seq
          (emit-failure :format "Expected list of length ~d, got ~s"
                        :args `(,(length criteria) ,l))))
      (loop for criterion in criteria for item in l do
        (let ((result (check-subcriterion-on-value criterion item)))
          (cond
            ((or (check-result-errors result) (check-result-failures result))
             (setf (check-result-info result) (append info (check-result-info result))
                   (check-result-warnings result)
                   (append warnings (check-result-warnings result)))
             (return-from seq result))
            (t (setf info (append info (check-result-info result))
                     warnings (append warnings (check-result-warnings result)))))))
      (check-result :info info :warnings warnings))))

(def-criterion (:permute (criterion) (l))
  (block permute-block
    (let ((perms (make-instance 'permuter :src l)))
      (loop while (has-next perms) do
        (let* ((x (next-permutation perms))
               (result (check-subcriterion-on-value criterion x)))
          (when (and (null (check-result-errors result))
                     (null (check-result-failures result)))
            (return-from permute-block (check-result)))))
      (emit-failure :format "No permutation of ~s satisfies ~s"
                    :args `(,l ,criterion)))))


(def-criterion (:across (&rest criteria) (v))
  (block across-block
    (let ((info nil) (warnings nil))
      (unless (eql (length v) (length criteria))
        (return-from across-block
          (emit-failure :format "Expected sequence of length ~d"
                        :args `(,(length criteria)))))
      (loop for criterion in criteria for idx from 0 do
        ;; (format t "Checking item ~d of vector ~s~%  against ~s~%" idx v criterion)
        (let ((result (check-subcriterion-on-value criterion (elt v idx))))
          ;; (format t "  result is ~s~%" result)
          (cond
            ((or (check-result-errors result) (check-result-failures result))
             (setf (check-result-info result)
                   (append info (check-result-info result))
                   (check-result-warnings result)
                   (append warnings (check-result-warnings result)))
             (return-from across-block result))
            (t (setf info (append info (check-result-info result))
                     warnings (append warnings
                                      (check-result-warnings result)))))))
      (check-result :info info :warnings warnings))))


(def-criterion (:slots (&rest clauses) (obj))
  (block slots-block
    (let ((warnings nil) (info nil))
      (loop for (slot criterion) in clauses do
        ;; (format t "Checking slot ~s of ~s~%  against ~s~%" slot obj criterion)
        (let ((value (slot-value obj slot)))
          ;; (format t "  value is ~s~%" value)
          (let ((result (check-subcriterion-on-value criterion value)))
            ;; (format t "  result is ~s~%" result)
            (cond
              ((or (check-result-errors result) (check-result-failures result))
               (setf (check-result-info result)
                     (append info (check-result-info result))
                     (check-result-warnings result)
                     (append warnings (check-result-warnings result)))
               (return-from slots-block result))
              (t
               (setf info (append info (check-result-info result))
                     warnings (append warnings
                                      (check-result-warnings result))))))))
      ;; (format t "End of slots loop~%")
      (check-result :info info :warnings warnings))))
