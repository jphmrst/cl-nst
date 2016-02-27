;;; File criteria.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Copyright (c) 2015-2016 John Maraist
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
(in-package :nst)

;;; Built-in basic testing criteria.

(def-criterion (:pass (:forms) :ignore)
  (make-success-report))
(setf (documentation* :pass 'criterion)
      "The =:pass= is a trivial test which always passes.

Example:
#+begin_example
\(def-test passing-test :pass 3 4 \"sd\")
#+end_example")

(def-criterion (:fail (:forms &rest args) :ignore)
  (make-failure-report :format (car args) :args (cdr args)))
(setf (documentation* :fail 'criterion)
      "The =:fail= criterion is a trivial test which always fails.  The format string and arguments should be suitable for the Lisp =format= function.

Usage:
#+begin_example
\(:fail FORMAT-STRING FORM ... FORM)
#+end_example
Example:
#+begin_example
\(def-test fails (:fail \"Expected a \~{}a\" \"string\") 312)
#+end_example")

(def-criterion (:warn (:forms &rest args) :ignore)
  (make-warning-report :format (car args) :args (cdr args)))
(setf (documentation* :warn 'criterion)
      "The =:warn= criterion issues a warning.  The format string and arguments should be suitable for the Lisp =format= function.

Usage:
#+begin_example
\(:warn FORMAT-STRING FORM ... FORM)
#+end_example
Example:
#+begin_example
\(:warn \"\~{}d is not a perfect square\" 5)
#+end_example")

(def-criterion (:true-form (:forms bool) :ignore)
  (if (eval bool)
      (make-success-report)
      (make-failure-report :format "Expected non-null result of ~s"
                           :args (list bool))))
(setf (documentation* :true-form 'criterion)
      "The =:true-form= criterion checks that a form evaluates to non-nil.")

(def-criterion (:true (:forms) (:values bool))
  (if bool
      (make-success-report)
      (make-failure-report :format "Expected non-null, got: ~s"
                           :args (list bool))))
(setf (documentation* :true 'criterion)
      "The =:true= criterion expects one form, which is evaluated at testing time; the criterion requires the result to be non-nil.")

(def-criterion (:eq (:forms target) (:values actual))
  (let ((targ-value (eval target)))
    (if (eq targ-value actual)
        (make-success-report)
      (make-failure-report :format "Value ~s not eq to value of ~s"
                           :args (list actual targ-value)))))
(setf (documentation* :eq 'criterion)
      "The =:eq= criterion checks a form using =eq=.  The criterion argument and the form under test are both evaluated at testing time.

Usage:
#+begin_example
\(:eq TARGET)
#+end_example
Example:
#+begin_example
\(def-test eq1 (:eq 'b) (cadr '(a b c)))
#+end_example")

(def-criterion-alias (:symbol name) `(:eq ',name))
(setf (documentation* :symbol 'criterion)
      "The =:symbol= criterion checks that its form under test evaluates to a symbol which is =eq= to the symbol name given as the criterion argument.

Usage:
#+begin_example
\(:symbol NAME)
#+end_example
Example:
#+begin_example
\(def-test sym1  (:symbol a) (car '(a b c)))
#+end_example
Example of a test which fails:
#+begin_example
\(def-test sym1x (:symbol a) (cadr '(a b c)))
#+end_example")

(def-criterion (:eql (:forms target) (:values actual))
  (if (eql (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not eql to value of ~s"
                           :args (list actual target))))
(setf (documentation* :eql 'criterion)
      "The =:eql= criterion checks a form using =eql=.  The criterion argument and the form under test are both evaluated at testing time.

Usage:
#+begin_example
\(:eql TARGET)
#+end_example
Example:
#+begin_example
\(def-test eql1 (:eql 2) (cadr '(1 2 3)))
#+end_example")

(def-criterion (:equal (:forms target) (:values actual))
  (if (equal (eval target) actual)
     (make-success-report)
     (make-failure-report :format "Value ~s not equal to value of ~s"
                          :args (list actual target))))
(setf (documentation* :equal 'criterion)
      "The =:equal= criterion checks a form using =eql=.  The criterion argument and the form under test are both evaluated at testing time.")

(def-criterion (:equalp (:forms target) (:values actual))
  (if (equalp (eval target) actual)
      (make-success-report)
      (make-failure-report :format "Value ~s not equalp to value of ~s"
                           :args (list actual target))))
(setf (documentation* :equalp 'criterion)
      "The =:equalp= criterion checks a form using =equalp=.  The criterion argument and the form under test are both evaluated at testing time.

Usage:
#+begin_example
\(:equalp TARGET)
#+end_example")

(def-criterion-alias (:forms-eq)    `(:predicate eq))
(setf (documentation* :forms-eq 'criterion)
      "The =:forms-eq= criterion compares its two forms under test using =eq=.  The forms are both evaluated at testing time.

Usage:
#+begin_example
\(:forms-eq)
#+end_example
Example:
#+begin_example
\(def-test eqforms1 :forms-eq (cadr '(a b c)) (caddr '(a c b)))
#+end_example")

(def-criterion-alias (:forms-eql)   `(:predicate eql))
(setf (documentation* :forms-eql 'criterion)
      "The =:forms-eql= criterion compares its two forms under test using =eql=.  The two forms under test are both evaluated at testing time.

Usage:
#+begin_example
\(:forms-eql)
#+end_example
Example:
#+begin_example
\(def-test eqlforms1 :forms-eql (cadr '(a 3 c)) (caddr '(a c 3)))
#+end_example")

(def-criterion-alias (:forms-equal) `(:predicate equal))
(setf (documentation* :forms-equal 'criterion)
      "The =:forms-equal= criterion compares its two forms under test using =equal=.  The forms are both evaluated at testing time.

Usage:
#+begin_example
\(:forms-equal)
#+end_example")

(def-criterion-alias (:value-list further) `(:apply list ,further))
(setf (documentation* :value-list 'criterion)
      "The =:value-list= criterion converts multiple values into a single list value.

Usage:
#+begin_example
\(:value-list FURTHER)
#+end_example")

(def-criterion (:predicate (:forms pred) (:values &rest vals))
  (if (apply (eval `(function ,pred)) vals)
      (make-success-report)
      (make-failure-report :format "Predicate ~s fails for ~s"
                           :args (list pred vals))))
(setf (documentation* :predicate 'criterion)
      "The =:predicate= criterion applies a predicate to the result of evaluating its form under test.  The criterion argument is a symbol (unquoted) or a lambda expression; at testing time, the forms under test are evaluated and passed to the denoted function.  The criterion expects that the result of the function is non-nil.

Usage:
#+begin_example
\(:predicate PRED)
#+end_example
Example:
#+begin_example
\(def-test pred1 (:predicate numberp) 3)
#+end_example
Example of a test which fails:
#+begin_example
\(def-test pred2 (:predicate eql) (+ 1 2) 3)
#+end_example")

(def-criterion-alias (:drop-values criterion)
  `(:apply (lambda (x &rest others) (declare (ignorable others)) x)
           ,criterion))
(setf (documentation* :drop-values 'criterion)
      "The =:drop-values= criterion checks the primary value according to the subordinate criterion, ignoring any additional returned values from the evaluation of the form under test.

Usage:
#+begin_example
\(:drop-values CRITERION)
#+end_example")

(def-criterion (:dump-forms (:forms blurb) (:values &rest forms))
  (block nil
    (format t "~%~a~%" blurb)
    (unless (listp forms)
      (return (make-failure-report :format "~s not a list"
                                   :args (list forms))))
    (loop for form in forms do (format t "~s~%" form))
    (make-failure-report :format "Arguments dumped" :args nil)))
(setf (documentation* :dump-forms 'criterion)
      "The =:dump-forms= criterion is for debugging NST criteria. It fails after writing the current forms to standard output.

Usage:
#+begin_example
\(:dump-forms BLURB)
#+end_example")

(def-criterion (:info (:forms string subcriterion) (:form expr-list-form))
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (push string (check-result-info subcheck))
    subcheck))
(setf (documentation* :info 'criterion)
      "The =:info= criterion adds an informational note to the check result.

Usage:
#+begin_example
\(:info STRING SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-test known-bug (:info \"Known bug\" (:eql 3)) 4)
#+end_example")

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
(setf (documentation* :err 'criterion)
      "The =:err= criterion evaluates the form under test, expecting the evaluation to raise some condition.  If the =class= argument is supplied, the criterion expects the raised condition to be a subclass.  Note that the name of the type should /not/ be quoted; it is not evaluated.

Usage:
#+begin_example
\(:err &KEY (TYPE CLASS))
#+end_example
Examples:
#+begin_example
\(def-test err1 (:err :type error) (error \"this should be caught\"))
(def-test err2 (:err) (error \"this should be caught\"))
#+end_example")

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
(setf (documentation* :perf 'criterion)
      "The =:perf= criterion evaluates the forms under test at testing time, checking that the evaluation completes within the given time limit.

Usage:
#+begin_example
\(:perf &KEY (MS MILLISECS) (SEC SECONDS) (MIN MINUTES))
#+end_example
Example:
#+begin_example
\(def-test perf1 (:perf :min 2) (ack 3 5))
#+end_example")

(def-criterion (:not (:forms subcriterion) (:form expr-list-form))
  (let ((subcheck (check-criterion-on-form subcriterion expr-list-form)))
    (cond
     ((check-result-errors subcheck)   subcheck)
     ((check-result-failures subcheck)
      (make-success-report :info (check-result-info subcheck)))
     (t (make-failure-report :format "Expected failure from ~s"
                             :args (list subcriterion))))))
(setf (documentation* :not 'criterion)
      "The =:not= criterion passes when testing according to =subcriterion= fails (but does not throw an error).

Usage:
#+begin_example
\(:not SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-test not1 (:not (:symbol b)) 'a)
#+end_example")

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
(setf (documentation* :all 'criterion)
      "The =:all= criterion brings several other criteria under one check, and verifies that they all pass.

Usage:
#+begin_example
\(:all SUBCRITERION ... SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-check not1 ()
    \(:all (:predicate even-p)
          \(:predicate prime-p))
  2)
#+end_example")

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
(setf (documentation* :any 'criterion)
      "The =:any= criterion passes when any of the subordinate criteria pass.

Usage:
#+begin_example
\(:any SUBCRITERION ... SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-check not1 ()
    \(:any (:predicate even-p)
          \(:predicate prime-p))
  5)
#+end_example")

(def-criterion (:apply (:forms transform criterion) (:form exprs-form))
  (check-criterion-on-form criterion
                           `(multiple-value-call #'list (apply #',transform ,exprs-form))))
(setf (documentation* :apply 'criterion)
      "The =:apply= criterion first evaluates the forms under test, applying =function= to them.  The overall criterion passes or fails exactly when the subordinate =criterion= with the application's multiple result values.

Usage:
#+begin_example
\(:apply FUNCTION CRITERION)
#+end_example
Example:
#+begin_example
\(def-test applycheck (:apply cadr (:eql 10)) '(0 10 20))
#+end_example")

(def-criterion (:check-err (:forms criterion) (:form forms))
  (block nil
    (let ((result (check-criterion-on-form criterion forms)))
      (cond
        ((check-result-errors result)
         (make-success-report))
        (t (unless (listp forms)
             (return (make-failure-report :format "~s not a list"
                                          :args (list forms))))
           (make-failure-report
            :format (named-function check-err-failure-handler
                      (lambda (s criterion forms)
                        (pprint-logical-block (s forms)
                          (format s "No expected error for check ~s on:"
                            criterion)
                          (loop for form in forms do
                                (pprint-newline :linear s)
                                (format s " ~s" form)))))
            :args (list criterion
                        (cond ((and (listp forms)
                                    (eq 'list (car forms)))
                               (cdr forms))
                              (t (list forms))))))))))
(setf (documentation* :check-err 'criterion)
      "The =:check-err= criterion is like =:err=, but proceeds according to the subordinate criterion rather than simply evaluating the input forms.

Usage:
#+begin_example
\(:check-err CRITERION)
#+end_example
Example:
#+begin_example
\(def-test check-err1
    \(:check-err :forms-eq)
  'asdfgh (error \"this should be caught\"))
#+end_example")

(def-criterion (:progn (:forms &rest forms-and-criterion) (:form forms))
  (let ((progn-forms (butlast forms-and-criterion))
        (criterion (car (last forms-and-criterion))))
    (loop for form in progn-forms do (eval form))
    (check-criterion-on-form criterion forms)))
(setf (documentation* :progn 'criterion)
      "The =:progn= criterion first evaluates the =FORM=s in order, and then proceeds with evaluation of the forms under test according to the subordinate criterion.

Usage:
#+begin_example
\(:progn FORM ... FORM SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-test form1 (:progn (setf zz 3) (:eql 3)) zz)
#+end_example")

(def-criterion (:proj (:forms indices criterion) (:values &rest values))
  (block nil
    (unless (listp indices)
      (return (make-failure-report :format "~s not a list"
                                   :args (list indices))))
    (check-criterion-on-form criterion
                             `(list ,@(loop for idx in indices
                                            collect `',(nth idx values))))))
(setf (documentation* :proj 'criterion)
      "The =:proj= criterion rearranges the forms under test by selecting a new list according to the index numbers into the old list.  Checking of the reorganized forms continues according to the subordinate criterion.

Usage:
#+begin_example
\(:proj INDICES CRITERION)
#+end_example
Example:
#+begin_example
\(def-test proj-1
    \(:proj (0 2) :forms-eq)
  'a 3 (car '(a b)))
#+end_example")

(def-criterion (:values (:forms &rest args) (:form form))
  (check-criterion-on-form `(:seq ,@args) `(list ,form)))
(setf (documentation* :values 'criterion)
      "The =:values= criterion checks each of the forms under test according to the respective subordinate criterion.

Usage:
#+begin_example
\(:values SUBCRITERION ... SUBCRITERION)
#+end_example")

(def-criterion (:each (:forms criterion) (:values l))
  (block each
    (let ((info nil) (warnings nil))
      (unless (listp l)
        (return-from each (make-failure-report :format "~s not a list"
                                               :args (list l))))
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
(setf (documentation* :each 'criterion)
      "The =:each= criterion evaluates the form under test, expecting to find a list as a result.  Expects that each argument of the list according to the subordinate =criterion=, and passes when all of these checks pass.

Usage:
#+begin_example
\(:each CRITERION)
#+end_example
Example:
#+begin_example
\(def-test each1 (:each (:symbol a)) '(a a a a a))
#+end_example")

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
(setf (documentation* :seq 'criterion)
      "The =:seq= criterion evaluates its input form, checks each of its elements according to the respective subordinate criterion, and passes when all of them pass.

Usage:
#+begin_example
\(:seq SUBCRITERION ... SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-check seqcheck
    (:seq (:predicate symbolp) (:eql 1) (:symbol d))
  '(a 1 d))
#+end_example")

(def-criterion (:alist* (:forms key-test-fn value-test-fn &rest specs)
                        (:values alist))
  (block checker
    (loop for (key val) in specs
        do (let ((pair (assoc key alist :test key-test-fn)))
             (cond
               ((null pair)
                (return-from checker
                  (make-failure-report :format "~s not found in association list"
                                       :args (list key))))
               (t (unless (funcall (eval `(function ,value-test-fn))
                                   (cdr pair) val)
                    (return-from checker
                      (make-failure-report
                       :format "For ~a value ~s does not match ~s in ~s"
                       :args (list key (cdr pair) val alist))))))))
    (make-success-report)))
(setf (documentation* :alist* 'criterion)
      "The =:alist*= criterion evaluates the form under test, expecting to find an association list as a result.  Using the two given function specs to test the keys (during retrieval, via =assoc=) and the values, the criterion enforces that the lists contains equivalent keys, mapping to respective equivalent values.  Note that the list may contain additional key/value pairs; see also =:alist=.

Usage:
#+begin_example
\(:alist* KEY-TEST-FN VALUE-TEST-FN (KEY VALUE) ... (KEY VALUE))
#+end_example")

(def-criterion-alias (:alist key-test-fn value-test-fn &rest specs)
  `(:all (:alist* ,key-test-fn ,value-test-fn ,@specs)
         (:drop-values (:apply length (:eql ,(length specs))))))
(setf (documentation* :alist 'criterion)
      "The =:alist= criterion evaluates the form under test, expecting to find an association list as a result.  Using the two given function specs to test the keys (during retrieval, via =assoc=) and the values, the criterion enforces that the association lists contains exactly equivalent keys, mapping to respective equivalent values.  Implemented using =:alist*= plus a check of the list =length=, which could be incorrect if the criterion lists duplicate keys.

Usage:
#+begin_example
\(:alist KEY-TEST-FN VALUE-TEST-FN (KEY VALUE) ... (KEY VALUE))
#+end_example")

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
(setf (documentation* :permute 'criterion)
      "The =:permute= criterion evaluates the form under test, expecting to find a list as a result.  The criterion expects to find that some permutation of this list will satisfy the subordinate criterion.

Usage:
#+begin_example
\(:permute CRITERION)
#+end_example
Example:
#+begin_example
\(def-test permute1 (:permute (:each (:eq 'a))) '(a a))
\(def-check permute2
    \(:permute (:seq (:symbol b)
                    \(:predicate symbolp)
                    \(:predicate numberp)))
  '(1 a b))
#+end_example")

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
(setf (documentation* :across 'criterion)
      "The =:across= criterion is like =:seq=, but for a vector instead of a list.

Usage:
#+begin_example
\(:across SUBCRITERION ... SUBCRITERION)
#+end_example
Example:
#+begin_example
\(def-check across1
    \(:across (:predicate symbolp) (:eql 1))
  \(vector 'a 1))
#+end_example")

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
(setf (documentation* :slots 'criterion)
      "The =:slots= criterion evaluates its input form, and passes when the value at each given slot satisfies the corresponding subordinate constraint.

Usage:
#+begin_example
\(:slots (SLOT-NAME SUBCRITERION) ... (SLOT-NAME SUBCRITERION))
#+end_example
Example:
#+begin_example
\(defclass classcheck ()
  \((s1 :initarg :s1 :reader get-s1)
   \(s2 :initarg :s2)
   \(s3 :initarg :s3)))
 \(def-test slot1
     \(:slots (s1 (:eql 10))
             \(s2 (:symbol zz))
             \(s3 (:seq (:symbol q) (:symbol w)
                       \(:symbol e) (:symbol r))))
   \(make-instance 'classcheck
     :s1 10 :s2 'zz :s3 '(q w e r)))
#+end_example")

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
(setf (documentation* :package-exports 'criterion)
      "

Usage:
#+begin_example
\(:package-exports PACKAGE-DESIG)
#+end_example")

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
(setf (documentation* :package-internal 'criterion)
      "

Usage:
#+begin_example
\(:package-internal PACKAGE-DESIG)
#+end_example")

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
(setf (documentation* :applying-common-criterion 'criterion)
      "The =:applying-common-criterion= criterion applies one criterion to several pairs of criterion arguments and data forms.

Usage:
#+begin_example
\(:applying-common-criterion [ CRITERION | (CRITERION ARG ... ARG) ]
                            ( ((ARG ... ARG) (FORM ... FORM))
                              ...
                              ((ARG ... ARG) (FORM ... FORM)) )
                            ...
                            ( ((ARG ... ARG) (FORM ... FORM))
                              ...
                              ((ARG ... ARG) (FORM ... FORM)) ) )
#+end_example")

(def-criterion-alias (:with-common-criterion criterion &body forms)
 `(:applying-common-criterion ,criterion
     ,@(loop for form in forms collect `(nil ,form))))
(setf (documentation* :with-common-criterion 'criterion)
      "The =:with-common-criterion= criterion applies one criterion to several data forms.

Usage:
#+begin_example
\(:with-common-criterion [ CRITERION | (CRITERION ARG ... ARG) ]
                        (FORM ...  FORM) ... (FORM ...  FORM) )
#+end_example")
