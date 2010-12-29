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

;;; Documentation generator for criteria.

(defdoc:def-target-type criterion ())

;;; Built-in basic testing criteria.

(def-criterion (:pass (:forms) :ignore)
  (make-success-report))
(defdoc:def-documentation (criterion :pass)
  (:intro (:latex "A trivial test, which always passes."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test passing-test :pass 3 4 \"sd\")"))))

(def-criterion (:fail (:forms &rest args) :ignore)
  (make-failure-report :format (car args) :args (cdr args)))
(defdoc:def-documentation (criterion :fail)
  (:callspec (format-string (:seq form)))
  (:intro (:latex "A trivial test, which always fails.  The format string and arguments should be suitable for the Lisp \\texttt{format} function."))
  (:details (:seq
          (:plain "Example:")
          (:code
           "(def-test fails (:fail \"Expected a \~{}a\" \"string\") 312)"))))

(def-criterion (:warn (:forms &rest args) :ignore)
  (make-warning-report :format (car args) :args (cdr args)))
(defdoc:def-documentation (criterion :warn)
  (:callspec (format-string (:seq form)))
  (:intro (:latex "Issue a warning.  The format string and arguments should be suitable for the Lisp \\texttt{format} function."))
  (:details (:seq
          (:plain "Example:")
          (:code "(:warn \"\~{}d is not a perfect square\" 5)"))))

(def-criterion (:true-form (:forms bool) :ignore)
  (if (eval bool)
      (make-success-report)
      (make-failure-report :format "Expected non-null, got: ~s"
                           :args (list bool))))
(defdoc:def-documentation (criterion :true-form)
  (:callspec (bool))
  (:intro (:latex ""))
;;;  (:details (:seq
;;;          (:plain "Example:")
;;;          (:code "")
;;;          (:plain "A example of a test which fails:")
;;;          (:code "")))
  )

(def-criterion (:true (:forms) (:values bool))
  (if bool
      (make-success-report)
      (make-failure-report :format "Expected non-null, got: ~s"
                           :args (list bool))))
(defdoc:def-documentation (criterion :true)
  (:callspec ())
  (:intro "Expects one form, which is evaluated at testing time; the criterion requires the result to be non-nil.")
;;;  (:details (:seq
;;;          (:plain "Example:")
;;;          (:code "")
;;;          (:plain "A example of a test which fails:")
;;;          (:code "")))
  )

(def-criterion (:eq (:forms target) (:values actual))
  (if (eq (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not eq to value of ~s"
                           :args `(,actual 'target))))
(defdoc:def-documentation (criterion :eq)
  (:callspec (target))
  (:intro (:latex "The criterion argument and the form under test are both evaluated at testing time; the criterion requires that the results be \\texttt{eq}."))
  (:details (:seq (:plain "Example:")
               (:code "(def-test eq1 (:eq 'b) (cadr '(a b c)))"))))

(def-criterion-alias (:symbol name) `(:eq ',name))
(defdoc:def-documentation (criterion :symbol)
  (:callspec (name))
  (:intro (:latex "The form under test is evaluated at testing time.  The criterion requires that the result be a symbol which is \\texttt{eq} to the symbol name given as the criterion argument."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test sym1  (:symbol a) (car '(a b c)))")
          (:plain "A example of a test which fails:")
          (:code "(def-test sym1x (:symbol a) (cadr '(a b c)))"))))

(def-criterion (:eql (:forms target) (:values actual))
  (if (eql (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not eql to value of ~s"
                           :args (list actual target))))
(defdoc:def-documentation (criterion :eql)
  (:callspec (target))
  (:intro (:latex "The criterion argument and the form under test are both evaluated at testing time; the criterion requires that the results be \\texttt{eql}."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eql1 (:eql 2) (cadr '(1 2 3)))"))))

(def-criterion (:equal (:forms target) (:values actual))
  (if (equal (eval target) actual)
     (make-success-report)
     (make-failure-report :format "Value ~s not equal to value of ~s"
                          :args (list actual target))))
(defdoc:def-documentation (criterion :equal)
  (:callspec (target))
  (:intro (:latex "The criterion argument and the form under test are both evaluated at testing time; the criterion requires that the results be \\texttt{equal}.")))

(def-criterion (:equalp (:forms target) (:values actual))
  (if (equalp (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not equalp to value of ~s"
                           :args (list actual target))))
(defdoc:def-documentation (criterion :equalp)
  (:callspec (target))
  (:intro (:latex "The criterion argument and the form under test are both evaluated at testing time; the criterion requires that the results be \\texttt{equalp}.")))

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(defdoc:def-documentation (criterion :forms-eq)
  (:callspec ())
  (:intro (:latex "The two forms under test are both evaluated at testing time; the criterion requires that the results be \\texttt{eq}."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eqforms1 :forms-eq (cadr '(a b c)) (caddr '(a c b)))"))))
(def-criterion-alias (:forms-eql)   `(:predicate eql))
(defdoc:def-documentation (criterion :forms-eql)
  (:callspec ())
  (:intro (:latex "The two forms under test are both evaluated at testing time; the criterion requires that the results be \\texttt{eql}."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eqlforms1 :forms-eql (cadr '(a 3 c)) (caddr '(a c 3)))"))))
(def-criterion-alias (:forms-equal) `(:predicate equal))
(defdoc:def-documentation (criterion :forms-equal)
  (:callspec ())
  (:intro (:latex "The two forms under test are both evaluated at testing time; the criterion requires that the results be \\texttt{equal}.")))
(def-criterion-alias (:value-list further) `(:apply list ,further))
(defdoc:def-documentation (criterion :value-list)
  (:callspec (further))
  (:intro (:latex "Converts multiple values into a single list value.")))

(def-criterion (:predicate (:forms pred) (:values &rest vals))
  (if (apply (eval `(function ,pred)) vals)
      (make-success-report)
      (make-failure-report :format "Predicate ~s fails for ~s"
                           :args (list pred vals))))
(defdoc:def-documentation (criterion :predicate)
  (:callspec (pred))
  (:intro (:latex "The criterion argument is a symbol (unquoted) or a lambda expression; at testing time, the forms under test are evaluated and passed to the denoted function.  The criterion expects that the result of the function is non-nil."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test pred1 (:predicate numberp) 3)")
          (:plain "A example of a test which fails:")
          (:code "(def-test pred2 (:predicate eql) (+ 1 2) 3)"))))

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))
(defdoc:def-documentation (criterion :drop-values)
  (:callspec (criterion))
  (:intro (:latex "Checks the primary value according to the subordinate criterion, ignoring any additional returned values from the evaluation of the form under test.")))

(def-criterion (:dump-forms (:forms blurb) (:values &rest forms))
  (format t "~%~a~%~{~s~%~}" blurb forms)
  (make-failure-report :format "Arguments dumped" :args nil))
(defdoc:def-documentation (criterion :dump-forms)
  (:callspec (blurb))
  (:intro (:latex "For debugging NST criteria: fails after writing the current forms to standard output.")))

(def-criterion (:info (:forms string subcriterion) (:form expr-list-form))
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (push string (check-result-info subcheck))
    subcheck))
(defdoc:def-documentation (criterion :info)
  (:callspec (string subcriterion))
  (:intro (:latex "Add an informational note to the check result."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test known-bug (:info \"Known bug\" (:eql 3)) 4)"))))

(def-criterion (:err (:forms &key (type 'error)) (:form expr-form))
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
(defdoc:def-documentation (criterion :err)
  (:callspec (&key (type CLASS)))
  (:intro (:latex "At testing time, evaluates the form under test, expecting the evaluation to raise some condition.  If the \\textit{CLASS} argument is supplied, the criterion expects the raised condition to be a subclass.  Note that the name of the type should \\emph{not} be quoted; it is not evaluated."))
  (:details (:seq
          (:plain "Examples:")
          (:code "(def-test err1 (:err :type error) (error \"this should be caught\"))")
          (:code "(def-test err2 (:err) (error \"this should be caught\"))"))))

(def-criterion (:perf (:forms &key (ms nil ms-supp-p) (sec nil sec-supp-p)
                              (min nil min-supp-p))
                      (:form expr-form))
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
(defdoc:def-documentation (criterion :perf)
  (:callspec (&key (ms MILLISECS) (sec SECONDS) (min MINUTES)))
  (:intro (:latex "Evaluates the forms under test at testing time, and expects the evaluation to complete within the given time limit."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test perf1 (:perf :min 2) (ack 3 5))"))))

(def-criterion (:not (:forms subcriterion) (:form expr-list-form))
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (cond
     ((check-result-errors subcheck)   subcheck)
     ((check-result-failures subcheck)
      (make-success-report :info (check-result-info subcheck)))
     (t (make-failure-report :format "Expected failure from ~s"
                             :args (list subcriterion))))))
(defdoc:def-documentation (criterion :not)
  (:callspec (subcriterion))
  (:intro (:latex "Passes when testing according to \\texttt{subcriterion} fails (but does not throw an error)."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test not1 (:not (:symbol b)) 'a)"))))

(def-criterion (:all (:forms &rest subcriteria) (:form expr-list-form))
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
(defdoc:def-documentation (criterion :all)
  (:callspec ((:seq subcriterion)))
  (:intro (:latex "This criterion brings several other criteria under one check, and verifies that they all pass."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-check not1 ()
    (:all (:predicate even-p)
          (:predicate prime-p))
  2)"))))

(def-criterion (:any (:forms &rest criteria) (:form expr-list-form))
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
(defdoc:def-documentation (criterion :any)
  (:callspec ((:seq subcriterion)))
  (:intro (:latex "Passes when any of the subordinate criteria pass."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-check not1 ()
    (:any (:predicate even-p)
          (:predicate prime-p))
  5)"))))

(def-criterion (:apply (:forms transform criterion) (:form exprs-form))
  (check-criterion-on-form criterion
                           `(multiple-value-call #'list (apply #',transform ,exprs-form))))
(defdoc:def-documentation (criterion :apply)
  (:callspec (FUNCTION CRITERION))
  (:intro (:latex "At testing time, first evaluates the forms under test, applying \\texttt{FUNCTION} to them.  The overall criterion passes or fails exactly when the subordinate \\texttt{CRITERION} with the application's multiple result values."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test applycheck (:apply cadr (:eql 10)) '(0 10 20))"))))

(def-criterion (:check-err (:forms criterion) (:form forms))
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
(defdoc:def-documentation (criterion :check-err)
  (:callspec (criterion))
  (:intro (:latex "Like \\texttt{:err}, but proceeds according to the subordinate criterion rather than simply evaluating the input forms."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test check-err1
    (:check-err :forms-eq)
  'asdfgh (error \"this should be caught\"))"))))

(def-criterion (:progn (:forms &rest forms-and-criterion) (:form forms))
  (let ((progn-forms (butlast forms-and-criterion))
        (criterion (car (last forms-and-criterion))))
    (loop for form in progn-forms do (eval form))
    (check-criterion-on-form criterion forms)))
(defdoc:def-documentation (criterion :progn)
  (:callspec ((:seq form) subcriterion))
  (:intro (:latex "At testing time, first evaluates the \\texttt{FORM}s in order, and then proceeds with evaluation of the forms under test according to the subordinate criterion."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test form1 (:progn (setf zz 3) (:eql 3)) zz)"))))

(def-criterion (:proj (:forms indices criterion) (:values &rest values))
  (check-criterion-on-form criterion
                           `(list ,@(loop for idx in indices collect `',(nth idx values)))))
(defdoc:def-documentation (criterion :proj)
  (:callspec (indices criterion))
  (:intro (:latex "Rearranges the forms under test by selecting a new list according to the index numbers into the old list.  Checking of the reorganized forms continues according to the subordinate criterion."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test proj-1
    (:proj (0 2) :forms-eq)
  'a 3 (car '(a b)))"))))

(def-criterion (:values (:forms &rest args) (:form form))
  (check-criterion-on-form `(:seq ,@args) `(list ,form)))
(defdoc:def-documentation (criterion :values)
  (:callspec ((:seq subcriterion)))
  (:intro (:latex "Checks each of the forms under test according to the respective subordinate criterion.")))

(def-criterion (:each (:forms criterion) (:values l))
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
(defdoc:def-documentation (criterion :each)
  (:callspec (criterion))
  (:intro (:latex "At testing time, evaluates the form under test, expecting to find a list as a result.  Expects that each argument of the list according to the subordinate \\texttt{criterion}, and passes when all of these checks pass."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test each1 (:each (:symbol a)) '(a a a a a))"))))

(def-criterion (:seq (:forms &rest criteria) (:values l))
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
(defdoc:def-documentation (criterion :seq)
  (:callspec ((:seq subcriterion)))
  (:intro (:latex "Evaluates its input form, checks each of its elements according to the respective subordinate criterion, and passes when all of them pass."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-check seqcheck
    (:seq (:predicate symbolp) (:eql 1) (:symbol d))
  '(a 1 d))"))))

(def-criterion (:permute (:forms criterion) (:values l))
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
(defdoc:def-documentation (criterion :permute)
  (:callspec (criterion))
  (:intro (:latex "At testing time, evaluates the form under test, expecting to find a list as a result.  The criterion expects to find that some permutation of this list will satisfy the subordinate criterion."))
  (:details (:seq
          (:plain "Examples:")
          (:code "(def-test permute1 (:permute (:each (:eq 'a))) '(a a))")
          (:code "(def-check permute2
    (:permute (:seq (:symbol b)
                    (:predicate symbolp)
                    (:predicate numberp)))
  '(1 a b))"))))

(def-criterion (:across (:forms &rest criteria) (:values v))
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
(defdoc:def-documentation (criterion :across)
  (:callspec ((:seq subcriterion)))
  (:intro (:latex "Like \\texttt{:seq}, but for a vector instead of a list."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-check across1
    (:across (:predicate symbolp) (:eql 1))
  (vector 'a 1))"))))

(def-criterion (:slots (:forms &rest clauses) (:values obj))
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
(defdoc:def-documentation (criterion :slots)
  (:callspec ((:seq (slot-name subcriterion))))
  (:intro (:latex "Evaluates its input form, and passes when the value at each given slot satisfies the corresponding subordinate constraint."))
  (:details (:seq
          (:plain "Example:")
          (:code "(defclass classcheck ()
  ((s1 :initarg :s1 :reader get-s1)
   (s2 :initarg :s2)
   (s3 :initarg :s3)))
 (def-test slot1
     (:slots (s1 (:eql 10))
             (s2 (:symbol zz))
             (s3 (:seq (:symbol q) (:symbol w)
                       (:symbol e) (:symbol r))))
   (make-instance 'classcheck
     :s1 10 :s2 'zz :s3 '(q w e r)))"))))

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

(def-criterion (:package-exports (:forms package-desig) (:values symbol-desig))
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
(defdoc:def-documentation (criterion :package-exports)
  (:callspec (package-desig)))

(def-criterion (:package-internal (:forms package-desig) (:values symbol-desig))
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
(defdoc:def-documentation (criterion :package-internal)
  (:callspec (package-desig)))

