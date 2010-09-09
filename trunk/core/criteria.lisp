;;; File criteria.lisp
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

;;; Built-in basic testing criteria.

(def-criterion-unevaluated (:pass () chk :ignore-forms t)
  (make-success-report))

(def-criterion-unevaluated (:fail (&rest args) chk :ignore-forms t)
  (make-failure-report :format (car args) :args (cdr args)))

(def-criterion-unevaluated (:warn (&rest args) chk :ignore-forms t)
  (make-warning-report :format (car args) :args (cdr args)))

(def-criterion (:true-form (bool) ())
  (if (eval bool)
      (make-success-report)
      (make-failure-report :format "Expected non-null, got: ~s"
                           :args (list bool))))

(def-criterion (:true () (bool))
  (if bool
      (make-success-report)
      (make-failure-report :format "Expected non-null, got: ~s"
                           :args (list bool))))

(def-criterion (:eq (target) (actual))
  (if (eq (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not eq to value of ~s"
                           :args `(,actual 'target))))

(def-criterion-alias (:symbol name) `(:eq ',name))

(def-criterion (:eql (target) (actual))
  (if (eql (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not eql to value of ~s"
                           :args (list actual target))))

(def-criterion (:equal (target) (actual))
  (if (equal (eval target) actual)
     (make-success-report)
     (make-failure-report :format "Value ~s not equal to value of ~s"
                          :args (list actual target))))

(def-criterion (:equalp (target) (actual))
  (if (equalp (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not equalp to value of ~s"
                           :args (list actual target))))

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(def-criterion-alias (:forms-eql)   `(:predicate eql))
(def-criterion-alias (:forms-equal) `(:predicate equal))
(def-criterion-alias (:value-list further) `(:apply list ,further))

(def-criterion (:predicate (pred) (&rest vals))
  (if (apply (eval `(function ,pred)) vals)
      (make-success-report)
      (make-failure-report :format "Predicate ~s fails for ~s"
                           :args (list pred vals))))

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))

(def-criterion (:dump-forms (blurb) (&rest forms))
  (format t "~%~a~%~{~s~%~}" blurb forms)
  (make-failure-report :format "Arguments dumped" :args nil))

(def-criterion-unevaluated (:info (string subcriterion) expr-list-form)
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (push string (check-result-info subcheck))
    subcheck))

(def-criterion-unevaluated (:err (&key (type 'error)) expr-form)
  (block err-criterion
    (format-at-verbosity 4 "    Setting up ~s handler for :err~%" type)
    (handler-bind-interruptable
        ((condition #'(lambda (e)
                        (cond
                         ((typep e type)
                          (format-at-verbosity 4
                              "Caught ~s as expected by :err criterion~%" e)
                          (return-from err-criterion (make-success-report)))
                         ((typep e 'error)
                          (format-at-verbosity 4
                              "Caught ~s but :err expected ~s~%" e type)
                          (unless *debug-on-error*
                            (return-from err-criterion
                              (make-error-report e))))))))
      (eval expr-form))
    (make-failure-report :format "~@<No expected error:~{~_ ~s~}~:>"
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
        (make-success-report)
        (make-failure-report
         :format "Execution time ~dms exceeded allowed time ~dms"
         :args (list elapsed-ms ms)))))
   (t
    (error ":perf check requires performance criteria specification"))))

(def-criterion-unevaluated (:not (subcriterion) expr-list-form)
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (cond
     ((check-result-errors subcheck)   subcheck)
     ((check-result-failures subcheck)
      (make-success-report :info (check-result-info subcheck)))
     (t (make-failure-report :format "Expected failure from ~s"
                             :args (list subcriterion))))))

(def-criterion-unevaluated (:all (&rest subcriteria) expr-list-form)
  (let ((*nst-context-evaluable* t))
    (declare (special *nst-context-evaluable*))
    (loop for subcriterion in subcriteria
          for subresult
            = (check-criterion-on-form subcriterion expr-list-form)
          append (check-result-warnings subresult) into warnings
          append (check-result-failures subresult) into failures
          append (check-result-errors subresult) into errors
          append (check-result-info subresult) into info
          finally (return (make-and-calibrate-check-result :warnings warnings
                                            :failures failures
                                            :errors errors :info info)))))

(def-criterion-unevaluated (:any (&rest criteria) expr-list-form)
  (let ((*nst-context-evaluable* t) (info nil))
    (block any-criterion
      (loop for criterion in criteria do
        ;; (format t "Trying ~s on ~s~%" criterion expr-list-form)
        (let* ((result (check-criterion-on-form criterion expr-list-form))
               (rf (check-result-failures result))
               (re (check-result-errors result))
               (ri (check-result-info result)))
          ;; (format t " - result is ~s~%" result)
          ;; (format t "     re ~s~%" re)
          ;; (format t "     rf ~s~%" rf)
          ;; (format t "     ri ~s~%" ri)
          (when re (setf info (nconc info re)))
          (when rf (setf info (nconc info rf)))
          (when ri (setf info (nconc info ri)))
          (unless (or re rf)
            (return-from any-criterion
              (make-success-report :info (nconc info
                                             (check-result-info result)))))))
      (make-failure-report :format "No disjuncts succeeded:~{~_ ~s~}"
                           :args (list criteria) :info info))))

(def-criterion-unevaluated (:apply (transform criterion) exprs-form)
  (check-criterion-on-form criterion
    `(multiple-value-call #'list (apply #',transform ,exprs-form))))

(def-criterion-unevaluated (:check-err (criterion) forms)
  (let ((result (check-criterion-on-form criterion forms)))
    (cond
     ((check-result-errors result)
      (make-success-report))
     (t (make-failure-report :format "~@<No expected error for check ~s on:~
                                 ~{~_ ~s~}~:>"
                             :args (list criterion
                                         (cond ((and (listp forms)
                                                     (eq 'list (car forms)))
                                                (cdr forms))
                                               (t (list forms)))))))))

;;;  (block check-err-block
;;;    (handler-bind-interruptable
;;;        ((error #'(lambda (e)
;;;                    (format-at-verbosity 4
;;;                        "Caught ~s as expected by :check-err~%" e)
;;;                    (return-from check-err-block (make-success-report)))))
;;;      (check-criterion-on-form criterion forms))
;;;    (make-failure-report :format "~@<No expected error for check ~s on:~
;;;                                 ~{~_ ~s~}~:>"
;;;                         :args (list criterion
;;;                                     (cond ((and (listp forms)
;;;                                                 (eq 'list (car forms)))
;;;                                            (cdr forms))
;;;                                           (t (list forms)))))))

(def-criterion-unevaluated (:progn (&rest forms-and-criterion) forms)
  (let ((progn-forms (butlast forms-and-criterion))
        (criterion (car (last forms-and-criterion))))
    (loop for form in progn-forms do (eval form))
    (check-criterion-on-form criterion forms)))

(def-criterion (:proj (indices criterion) (&rest values))
  (check-criterion-on-form criterion
    `(list ,@(loop for idx in indices collect `',(nth idx values)))))

(def-criterion-unevaluated (:values (&rest args) form)
    (check-criterion-on-form `(:seq ,@args) `(list ,form)))

(def-criterion (:each (criterion) (l))
  (block each
    (let ((info nil) (warnings nil))
      (loop for value in l do
        (let ((result (check-criterion-on-form criterion `(list ',value))))
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
      (make-success-report :info info :warnings warnings))))

(def-criterion (:seq (&rest criteria) (l))
  (block seq
    (let ((info nil) (warnings nil))
      (unless (eql (length l) (length criteria))
        (return-from seq
          (make-failure-report :format "Expected list of length ~d, got ~s"
                               :args `(,(length criteria) ,l))))
      (loop for criterion in criteria for item in l do
        (let ((result (check-criterion-on-value criterion item)))
          (cond
            ((or (check-result-errors result) (check-result-failures result))
             (setf (check-result-info result)
                   (append info (check-result-info result))
                   (check-result-warnings result)
                   (append warnings (check-result-warnings result)))
             (return-from seq result))
            (t (setf info (append info (check-result-info result))
                     warnings (append warnings
                                      (check-result-warnings result)))))))
      (make-success-report :info info :warnings warnings))))

(def-criterion (:permute (criterion) (l))
  (block permute-block
    (let ((perms (make-instance 'permuter :src l)))
      (loop while (has-next perms) do
        (let* ((x (next-permutation perms))
               (result (check-criterion-on-value criterion x)))
          (when (and (null (check-result-errors result))
                     (null (check-result-failures result)))
            (return-from permute-block (make-success-report)))))
      (make-failure-report :format "No permutation of ~s satisfies ~s"
                           :args `(,l ,criterion)))))

(def-criterion (:across (&rest criteria) (v))
  (block across-block
    (let ((info nil) (warnings nil))
      (unless (eql (length v) (length criteria))
        (return-from across-block
          (make-failure-report :format "Expected sequence of length ~d"
                               :args `(,(length criteria)))))
      (loop for criterion in criteria for idx from 0 do
        (let ((result (check-criterion-on-value criterion (elt v idx))))
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
      (make-success-report :info info :warnings warnings))))

(def-criterion (:slots (&rest clauses) (obj))
  (block slots-block
    (let ((warnings nil) (info nil))
      (loop for (slot criterion) in clauses do
        (let ((value (slot-value obj slot)))
          (let ((result (check-criterion-on-value criterion value)))
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
      (make-success-report :info info :warnings warnings))))

(defun refine-package-symbol-desigs (package-desig symbol-desig)
  (block refiner
    (let ((package
           (cond
             ((packagep package-desig) package-desig)
             ((symbolp package-desig)
              (find-package (symbol-name package-desig)))
             ((stringp package-desig)
              (find-package package-desig))
             (t (return-from refiner
                  (values (make-failure-report
                           :format "~s does not designate a package"
                           :args (list package-desig))
                          nil nil)))))
          (symbol-string
           (cond
             ((symbolp symbol-desig) (symbol-name symbol-desig))
             ((stringp symbol-desig) symbol-desig)
             (t (return-from refiner
                  (values (make-failure-report
                           :format "~s does not designate a symbol"
                           :args (list symbol-desig))
                          nil nil))))))
      (values nil package symbol-string))))

(def-criterion (:package-exports (package-desig) (symbol-desig))
  (block crit
    (multiple-value-bind (fail package symbol-string)
        (refine-package-symbol-desigs package-desig symbol-desig)
      (when fail (return-from crit fail))
      (multiple-value-bind (actual status) (find-symbol symbol-string package)
        (declare (ignore actual))
        (case status
          ((nil) (make-failure-report
                  :format "No symbol ~s in package ~a"
                  :args (list symbol-string (package-name package))))
          ((:inherited) (make-failure-report
                         :format "Symbol ~s is inherited but not exported by ~a"
                         :args (list symbol-string (package-name package))))
          ((:external) (make-success-report))
          ((:internal) (make-failure-report
                        :format "Symbol ~s is internal, not exported, in ~a"
                        :args (list symbol-string (package-name package))))
          (otherwise (make-error-report
                      :format "Unexpected status result from find-symbol ~s"
                      :args (list status))))))))

(def-criterion (:package-internal (package-desig) (symbol-desig))
  (block crit
    (multiple-value-bind (fail package symbol-string)
        (refine-package-symbol-desigs package-desig symbol-desig)
      (when fail (return-from crit fail))
      (multiple-value-bind (actual status) (find-symbol symbol-string package)
        (declare (ignore actual))
        (case status
          ((nil) (make-failure-report
                  :format "No symbol ~s in package ~a"
                  :args (list symbol-string (package-name package))))
          ((:inherited) (make-failure-report
                         :format
                         "Symbol ~s is inherited, but not exported, by ~a"
                         :args (list symbol-string (package-name package))))
          ((:external) (make-failure-report
                        :format "Symbol ~s is exported, not internal, in ~a"
                        :args (list symbol-string (package-name package))))
          ((:internal) (make-success-report))
          (otherwise (make-error-report
                      :format "Unexpected result from find-symbol")))))))

