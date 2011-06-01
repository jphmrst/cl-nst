;;; File criteria.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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

(defdoc:def-target-type criterion (:symbol-definition-nocheck t))

;;; Built-in basic testing criteria.

(def-criterion (:pass (:forms) :ignore)
  (make-success-report))
(defdoc:def-documentation (criterion :pass)
  (:properties (nst-manual misc-criteria))
  (:intro (:seq "The " (:lisp criterion :pass) (:latex " is a trivial test, which always passes.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(def-test passing-test :pass 3 4 \"sd\")"))))

(def-criterion (:fail (:forms &rest args) :ignore)
  (make-failure-report :format (car args) :args (cdr args)))
(defdoc:def-documentation (criterion :fail)
  (:callspec (format-string (:seq form)))
  (:intro (:seq "The " (:lisp criterion :fail) (:latex " criterion is a trivial test which always fails.  The format string and arguments should be suitable for the Lisp \\texttt{format} function.")))
  (:details (:seq
          (:plain "Example:")
          (:code
           "(def-test fails (:fail \"Expected a \~{}a\" \"string\") 312)"))))

(def-criterion (:warn (:forms &rest args) :ignore)
  (make-warning-report :format (car args) :args (cdr args)))
(defdoc:def-documentation (criterion :warn)
  (:properties (nst-manual misc-criteria))
  (:callspec (format-string (:seq form)))
  (:intro (:seq "The " (:lisp criterion :warn) (:latex " criterion issues a warning.  The format string and arguments should be suitable for the Lisp \\texttt{format} function.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(:warn \"\~{}d is not a perfect square\" 5)"))))

(def-criterion (:true-form (:forms bool) :ignore)
  (if (eval bool)
      (make-success-report)
      (make-failure-report :format "Expected non-null result of ~s"
                           :args (list bool))))
(defdoc:def-documentation (criterion :true-form)
  (:callspec (bool))
  (:intro (:seq "The " (:lisp criterion :true-form) (:latex " criterion checks that a form evaluates to non-nil")))
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
    (:properties (nst-manual basic-criteria))
  (:callspec ())
  (:intro (:seq "The " (:lisp criterion :true) (:latex " criterion expects one form, which is evaluated at testing time; the criterion requires the result to be non-nil.")))
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
    (:properties (nst-manual basic-criteria))
  (:callspec (target))
  (:intro (:seq "The " (:lisp criterion :eq) " criterion checks a form using " (:lisp function eq) ".  The criterion argument and the form under test are both evaluated at testing time."))
  (:details (:seq (:plain "Example:")
               (:code "(def-test eq1 (:eq 'b) (cadr '(a b c)))"))))

(def-criterion-alias (:symbol name) `(:eq ',name))
(defdoc:def-documentation (criterion :symbol)
    (:properties (nst-manual basic-criteria))
  (:callspec (name))
  (:intro (:seq "The " (:lisp criterion :symbol) " criterion checks that its form under test evaluates to a symbol which is " (:lisp function eq) " to the symbol name given as the criterion argument."))
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
    (:properties (nst-manual basic-criteria))
  (:callspec (target))
  (:intro (:seq "The " (:lisp criterion :eql) " criterion checks a form using " (:lisp function eql) ".  The criterion argument and the form under test are both evaluated at testing time."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eql1 (:eql 2) (cadr '(1 2 3)))"))))

(def-criterion (:equal (:forms target) (:values actual))
  (if (equal (eval target) actual)
     (make-success-report)
     (make-failure-report :format "Value ~s not equal to value of ~s"
                          :args (list actual target))))
(defdoc:def-documentation (criterion :equal)
    (:properties (nst-manual basic-criteria))
  (:callspec (target))
  (:intro (:seq "The " (:lisp criterion :equal) " criterion checks a form using " (:lisp function eql) ".  The criterion argument and the form under test are both evaluated at testing time.")))

(def-criterion (:equalp (:forms target) (:values actual))
  (if (equalp (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not equalp to value of ~s"
                           :args (list actual target))))
(defdoc:def-documentation (criterion :equalp)
    (:properties (nst-manual basic-criteria))
  (:callspec (target))
  (:intro (:seq "The " (:lisp criterion :equalp) " criterion checks a form using " (:lisp function equalp) ".  The criterion argument and the form under test are both evaluated at testing time.")))

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(defdoc:def-documentation (criterion :forms-eq)
    (:properties (nst-manual basic-criteria))
  (:callspec ())
  (:intro (:seq "The " (:lisp criterion :forms-eq) " criterion compares its two forms under test using " (:lisp function eq) ".  The forms are both evaluated at testing time."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eqforms1 :forms-eq (cadr '(a b c)) (caddr '(a c b)))"))))
(def-criterion-alias (:forms-eql)   `(:predicate eql))
(defdoc:def-documentation (criterion :forms-eql)
    (:properties (nst-manual basic-criteria))
  (:callspec ())
  (:intro (:seq "The " (:lisp criterion :forms-eql) " criterion compares its two forms under test using " (:lisp function eql) ".  The two forms under test are both evaluated at testing time."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test eqlforms1 :forms-eql (cadr '(a 3 c)) (caddr '(a c 3)))"))))
(def-criterion-alias (:forms-equal) `(:predicate equal))
(defdoc:def-documentation (criterion :forms-equal)
    (:properties (nst-manual basic-criteria))
  (:callspec ())
  (:intro (:seq "The " (:lisp criterion :forms-equal) " criterion compares its two forms under test using " (:lisp function equal) ".  The forms are both evaluated at testing time.")))
(def-criterion-alias (:value-list further) `(:apply list ,further))
(defdoc:def-documentation (criterion :value-list)
  (:properties (nst-manual multiple-values-criteria))
  (:callspec (further))
  (:intro :seq "The" (:lisp criterion :value-list) " criterion converts multiple values into a single list value."))

(def-criterion (:predicate (:forms pred) (:values &rest vals))
  (if (apply (eval `(function ,pred)) vals)
      (make-success-report)
      (make-failure-report :format "Predicate ~s fails for ~s"
                           :args (list pred vals))))
(defdoc:def-documentation (criterion :predicate)
    (:properties (nst-manual basic-criteria))
  (:callspec (pred))
  (:intro (:seq "The " (:lisp criterion :predicate) " criterion applies a predicate to the result of evaluating its form under test.  The criterion argument is a symbol (unquoted) or a lambda expression; at testing time, the forms under test are evaluated and passed to the denoted function.  The criterion expects that the result of the function is non-nil."))
  (:details (:seq
          (:plain "Example:")
          (:code "(def-test pred1 (:predicate numberp) 3)")
          (:plain "A example of a test which fails:")
          (:code "(def-test pred2 (:predicate eql) (+ 1 2) 3)"))))

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))
(defdoc:def-documentation (criterion :drop-values)
  (:properties (nst-manual multiple-values-criteria))
  (:callspec (criterion))
  (:intro (:seq "The " (:lisp criterion :drop-values) " criterion checks the primary value according to the subordinate criterion, ignoring any additional returned values from the evaluation of the form under test.")))

(def-criterion (:dump-forms (:forms blurb) (:values &rest forms))
  (format t "~%~a~%" blurb)
  (loop for form in forms do (format t "~s~%" form))
  (make-failure-report :format "Arguments dumped" :args nil))
(defdoc:def-documentation (criterion :dump-forms)
  (:properties (nst-manual misc-criteria))
  (:callspec (blurb))
  (:intro (:seq "The " (:lisp criterion :dump-forms) " criterion is for debugging NST criteria. It fails after writing the current forms to standard output.")))

(def-criterion (:info (:forms string subcriterion) (:form expr-list-form))
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (push string (check-result-info subcheck))
    subcheck))
(defdoc:def-documentation (criterion :info)
  (:properties (nst-manual misc-criteria))
  (:callspec (string subcriterion))
  (:intro (:seq "The " (:lisp criterion :info) (:latex " criterion adds an informational note to the check result.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(def-test known-bug (:info \"Known bug\" (:eql 3)) 4)"))))

(def-criterion (:err (:forms &key (type 'error)) (:form expr-form))
  (block err-criterion
    (format-at-verbosity 4 "    Setting up ~s handler for :err~%" type)
    (handler-bind-interruptable
        ((condition (named-function err-criterion-condition-handler
                      (lambda (e)
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
                               (make-error-report e)))))))))
      (eval expr-form))
    (make-failure-report :format (named-function errcriterion-failure-formatter
                                   (lambda (stream forms)
                                     (pprint-logical-block (stream forms)
                                       (write "No expected error:" :stream stream)
                                       (loop for form in forms do
                                             (pprint-newline :linear stream)
                                             (format stream " ~s" form)))))
                         :args `(,(cond
                                    ((and (listp expr-form)
                                          (eq 'list (car expr-form)))
                                     (cdr expr-form))
                                    (t (list expr-form)))))))
(defdoc:def-documentation (criterion :err)
    (:properties (nst-manual basic-criteria))
  (:callspec (&key (type CLASS)))
  (:intro (:seq "The " (:lisp criterion :err) (:latex " criterion evaluates the form under test, expecting the evaluation to raise some condition.  If the \\textit{CLASS} argument is supplied, the criterion expects the raised condition to be a subclass.  Note that the name of the type should \\emph{not} be quoted; it is not evaluated.")))
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
    (:properties (nst-manual basic-criteria))
  (:callspec (&key (ms MILLISECS) (sec SECONDS) (min MINUTES)))
  (:intro (:seq "The " (:lisp criterion :perf) " criterion evaluates the forms under test at testing time, checking that the evaluation completes within the given time limit."))
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
    (:properties (nst-manual compound-criteria))
  (:callspec (subcriterion))
  (:intro (:seq "The " (:lisp criterion :not) (:latex " criterion passes when testing according to \\texttt{subcriterion} fails (but does not throw an error).")))
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
  (:properties (nst-manual compound-criteria))
  (:callspec ((:seq subcriterion)))
  (:intro (:seq "The " (:lisp criterion :all) (:latex " criterion brings several other criteria under one check, and verifies that they all pass.")))
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
      (make-failure-report
       :format (named-function any-criterion-failure-handler
                 (lambda (stream disjuncts)
                   (pprint-logical-block (stream disjuncts)
                     (write "No disjuncts succeeded:" :stream stream)
                     (loop for disjunct in disjuncts do
                           (pprint-newline :linear stream)
                           (format stream " ~s" disjunct)))))
        :args (list criteria)
        :info info))))
(defdoc:def-documentation (criterion :any)
  (:properties (nst-manual compound-criteria))
  (:callspec ((:seq subcriterion)))
  (:intro (:seq "The " (:lisp criterion :any) (:latex " criterion passes when any of the subordinate criteria pass.")))
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
  (:properties (nst-manual compound-criteria))
  (:callspec (FUNCTION CRITERION))
  (:intro (:seq "The " (:lisp criterion :apply) (:latex " criterion first evaluates the forms under test, applying \\texttt{FUNCTION} to them.  The overall criterion passes or fails exactly when the subordinate \\texttt{CRITERION} with the application's multiple result values.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(def-test applycheck (:apply cadr (:eql 10)) '(0 10 20))"))))

(def-criterion (:check-err (:forms criterion) (:form forms))
  (let ((result (check-criterion-on-form criterion forms)))
    (cond
     ((check-result-errors result)
      (make-success-report))
     (t (make-failure-report
         :format (named-function check-err-failure-handler
                   (lambda (s criterion forms)
                     (pprint-logical-block (s forms)
                       (format s "No expected error for check ~s on:" criterion)
                       (loop for form in forms do
                             (pprint-newline :linear s)
                             (format s " ~s" form)))))
         :args (list criterion
                     (cond ((and (listp forms)
                                 (eq 'list (car forms)))
                            (cdr forms))
                           (t (list forms)))))))))
(defdoc:def-documentation (criterion :check-err)
  (:properties (nst-manual compound-criteria))
  (:callspec (criterion))
  (:intro (:seq "The " (:lisp criterion :check-err) (:latex " criterion is like \\texttt{:err}, but proceeds according to the subordinate criterion rather than simply evaluating the input forms.")))
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
  (:properties (nst-manual compound-criteria))
  (:callspec ((:seq form) subcriterion))
  (:intro (:seq "The " (:lisp criterion :progn) (:latex " criterion first evaluates the \\texttt{FORM}s in order, and then proceeds with evaluation of the forms under test according to the subordinate criterion.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(def-test form1 (:progn (setf zz 3) (:eql 3)) zz)"))))

(def-criterion (:proj (:forms indices criterion) (:values &rest values))
  (check-criterion-on-form criterion
                           `(list ,@(loop for idx in indices collect `',(nth idx values)))))
(defdoc:def-documentation (criterion :proj)
  (:properties (nst-manual compound-criteria))
  (:callspec (indices criterion))
  (:intro (:seq "The " (:lisp criterion :proj) (:latex " criterion rearranges the forms under test by selecting a new list according to the index numbers into the old list.  Checking of the reorganized forms continues according to the subordinate criterion.")))
  (:details (:seq
             (:plain "Example:")
             (:code "(def-test proj-1
    (:proj (0 2) :forms-eq)
  'a 3 (car '(a b)))"))))

(def-criterion (:values (:forms &rest args) (:form form))
  (check-criterion-on-form `(:seq ,@args) `(list ,form)))
(defdoc:def-documentation (criterion :values)
  (:properties (nst-manual multiple-values-criteria))
  (:callspec ((:seq subcriterion)))
  (:intro ":seq " (:lisp criterion :values) " criterion (The is) checks each of the forms under test according to the respective subordinate criterion."))

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
  (:properties (nst-manual list-criteria))
  (:callspec (criterion))
  (:intro (:seq "The " (:lisp criterion :each) (:latex " criterion evaluates the form under test, expecting to find a list as a result.  Expects that each argument of the list according to the subordinate \\texttt{criterion}, and passes when all of these checks pass.")))
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
  (:properties (nst-manual list-criteria))
  (:callspec ((:seq subcriterion)))
  (:intro (:seq "The " (:lisp criterion :seq) (:latex " criterion evaluates its input form, checks each of its elements according to the respective subordinate criterion, and passes when all of them pass.")))
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
  (:properties (nst-manual list-criteria))
  (:callspec (criterion))
  (:intro (:seq "The " (:lisp criterion :permute) (:latex " criterion evaluates the form under test, expecting to find a list as a result.  The criterion expects to find that some permutation of this list will satisfy the subordinate criterion.")))
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
  (:properties (nst-manual vector-criteria))
  (:callspec ((:seq subcriterion)))
  (:intro (:seq "The " (:lisp criterion :across) (:latex " criterion is like \\texttt{:seq}, but for a vector instead of a list.")))
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
  (:properties (nst-manual class-criteria))
  (:callspec ((:seq (slot-name subcriterion))))
  (:intro (:seq "The " (:lisp criterion :slots) (:latex " criterion evaluates its input form, and passes when the value at each given slot satisfies the corresponding subordinate constraint.")))
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

(def-criterion (:applying-common-criterion (:forms criterion-head &body data-sets) :ignore)
  (let ((criterion-list-prefix (cond
                                 ((symbolp criterion-head)
                                  (list criterion-head))
                                 ((listp criterion-head) criterion-head)
                                 (t (error "Expected symbol or list, got ~s"
                                           criterion-head)))))
    (loop for (criterion-args data-forms) in data-sets
          for subresult = (check-criterion-on-form
                                (append criterion-list-prefix criterion-args)
                                `(list ,@data-forms))
        append (check-result-warnings subresult) into warnings
        append (check-result-failures subresult) into failures
        append (check-result-errors subresult) into errors
        append (check-result-info subresult) into info
        finally (return (make-and-calibrate-check-result :warnings warnings
                                                         :failures failures
                                                         :errors errors
                                                         :info info)))))
(defdoc:def-documentation (criterion :applying-common-criterion)
  (:properties (nst-manual compound-criteria))
  (:intro (:seq "The " (:lisp criterion :applying-common-criterion) " criterion applies one criterion to several pairs of criterion arguments and data forms."))
  (:callspec ((:alt criterion (criterion (:seq arg)))
              (:seq ( (:seq (((:seq arg)) ((:seq form)))))))))

(def-criterion-alias (:with-common-criterion criterion &body forms)
  `(:applying-common-criterion ,criterion
     ,@(loop for form in forms collect `(nil ,form))))
(defdoc:def-documentation (criterion :with-common-criterion)
  (:properties (nst-manual compound-criteria))
  (:intro (:seq "The " (:lisp criterion :with-common-criterion) " criterion applies one criterion to several data forms."))
  (:callspec ((:alt criterion (criterion (:seq arg))) (:seq ((:seq form))))))
