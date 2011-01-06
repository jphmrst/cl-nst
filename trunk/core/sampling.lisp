;;; File sampling.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009-2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
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

(defvar +arbitrary-generable-types+ (make-hash-table))
(defvar +scalar-generable-types+ (make-hash-table))

(defmacro coin-flip (&rest outcomes)
  (let ((len (length outcomes))
        (flip (gensym)))
    `(let ((,flip (random ,(* 100 len))))
       (cond
         ,@(loop for outcome in outcomes
                 for step from 1
                 collect
                 `((<= ,flip ,(* step 100))
                     ,outcome))))))

(defvar *current-compound-structure-depth* 0)
(defvar *max-compound-structure-depth* 4)
(def-documentation (variable *max-compound-structure-depth*)
  (:tags sample)
  (:properties (api-summary sample))
    (:blurb (:latex "The \\texttt{*max-compound-structure-depth*} variable sets the maximum nesting depth of compound data structures: beyond that depth, \\texttt{scalar} rather than \\texttt{t} is the default element generator.  This restriction does not apply to explicitly specified element types, only to the use of defaults.")))

(defmacro compound-structure (&body forms)
  `(let ((*current-compound-structure-depth*
          (+ 1 *current-compound-structure-depth*)))
     ,@forms))
(def-documentation (compiler-macro compound-structure)
  (:tags sample)
  (:properties (api-summary sample))
    (:blurb (:latex "The \\texttt{compound-structure} macro wraps substructure which should be considered compound for the limits set by \\texttt{*max-compound-structure-depth*}.")))

(defgeneric arbitrary (typ)
  (:method ((spec cons))
     (apply #'arbitrary-by-spec (car spec) (cdr spec)))
  (:method (other)
     (arbitrary-by-spec other)))
(def-documentation (function arbitrary)
  (:tags sample)
  (:properties (api-summary sample))
    (:intro (:latex "This function takes a single argument, which determines the type of the value to be generated.  For simple types, the name of the type (or the class object, such as returned by \\texttt{find-class}) by itself is a complete specification.  For more complicated types, \\texttt{arbitrary} can also take a list argument, where the first element gives the type and the remaining elements are keyword argument providing additional requirements for the generated value.")))


(define-nst-error unknown-arbitrary-domain
    ((domain  :initarg :domain  :reader domain)
     (keyword-args :initarg :keyword-args :reader keyword-args))
  (stream cnd)
  (format stream "Cannot generate an arbitrary value of type: ~s~{ ~s~}"
    (domain cnd) (keyword-args cnd)))

(defgeneric arbitrary-by-spec (name &key &allow-other-keys)
  (:documentation "Return an arbitrary value according to a spec")

  (:method (unknown &rest keyword-args)
     (error 'unknown-arbitrary-domain
       :domain unknown :keyword-args keyword-args))

  (:method ((other-named-spec symbol) &rest keyword-args &key &allow-other-keys)
     (apply #'arbitrary-by-spec (find-class other-named-spec) keyword-args))

  (:method ((any (eql (find-class t))) &key)
     (arbitrary (pick-from-sequence (arbitrary-generable-types)))))

(defmacro def-arbitrary-instance-type (type-expr &body technique)

  (let (type-spec self keyargs scalar-p)
    (cond
     ((symbolp type-expr)
      (setf type-spec type-expr self (gensym)))
     ((listp type-expr)
      (destructuring-bind (typ &key (param (gensym)) key scalar)
          type-expr
        (setf  type-spec typ
               self param
               keyargs key
               scalar-p scalar)))
     (t (error 'not-expected-form
          :expected-form "symbol or list" :actual-value type-expr)))

    (let ((better-spec (find-class type-spec nil)))
      (when better-spec
        (setf type-spec better-spec)))

    (when (symbolp type-spec)
      (setf type-spec `',type-spec))

    `(progn
       (defmethod arbitrary-by-spec ((,self (eql ,type-spec)) &key ,@keyargs)
         ,@technique)
       ,@(when scalar-p
           `((setf (gethash ,type-spec +scalar-generable-types+) t)))
       (setf (gethash ,type-spec +arbitrary-generable-types+) t))))
(def-documentation (compiler-macro def-arbitrary-instance-type)
  (:tags sample)
  (:properties (api-summary sample))
    (:intro (:latex "New type specifications for invariant-testing. are defined with the \\texttt{def-arbitrary-instance-type}\\indexLisp{def-arbitrary-instance-type} macro."))
  (:callspec ((spec-name &key (params formals) (scalar bool) (key key))
              &body (:seq form)))
  (:params (formals (:latex "Formal parameter definition used to pass subcomponent types."))
           (scalar (:seq
                    (:latex "When a non-null value is provided for the \\texttt{:scalar} argument, the new specifier is taken to be generable by the \\texttt{scalar} specification.")
                    (:code "  (def-arbitrary-instance-type (ratio :scalar t)
    (/ (arbitrary 'integer)
       (let ((raw (arbitrary (find-class 'integer))))
         (cond
           ((< raw 0) raw)
           (t (+ 1 raw))))))")))
           (key (:seq
                 (:latex "The \\texttt{:key} argument gives a list of keyword arguments which may accompany the new specification.  For the \\texttt{cons} type, keyword arguments allow specifications for the left and right components:")
                 (:code "  (def-arbitrary-instance-type (cons :key ((car t car-supp-p)
                                           (cdr t cdr-supp-p)))
    (compound-structure
     (when (and (not car-supp-p)
                (>= *current-compound-structure-depth*
                    *max-compound-structure-depth*))
       (setf car 'scalar))
     (when (and (not cdr-supp-p)
                (>= *current-compound-structure-depth*
                    *max-compound-structure-depth*))
       (setf cdr 'scalar))
     (cons (arbitrary car) (arbitrary cdr))))")))
           (form (:latex "Construct and return (as if through progn) the arbtrary instance."))))

(def-arbitrary-instance-type (number :param n)
    (arbitrary (arbitrary-grounded-type n)))
(def-arbitrary-instance-type (real :param n)
    (arbitrary (arbitrary-grounded-type n)))
(def-arbitrary-instance-type (rational :param n)
    (arbitrary (arbitrary-grounded-type n)))
(def-arbitrary-instance-type (integer :param n)
    (arbitrary (arbitrary-grounded-type n)))
(def-arbitrary-instance-type (float :param n)
    (arbitrary (arbitrary-grounded-type n)))

(def-arbitrary-instance-type scalar
    (arbitrary (pick-from-sequence
                (loop for ty being the hash-keys of +scalar-generable-types+
                      collect ty))))

(def-arbitrary-instance-type (fixnum :scalar t)
     (coin-flip (random most-positive-fixnum)
                (- 0 (random (- 0 most-negative-fixnum)))))

(def-arbitrary-instance-type (bignum :scalar t)
    (let ((factors (+ 1 (floor (/ 1 (- 1 (* (random 1.0) (random 1.0)))))))
          (result 1))
      (loop for z from 1 to factors do
        (setf result (* result (arbitrary #-(or clisp ecl) (find-class 'fixnum)
                                          #+(or clisp ecl) 'fixnum))))
      result))

(def-arbitrary-instance-type (ratio :scalar t)
    (/ (arbitrary (find-class 'integer))
       (let ((raw (arbitrary (find-class 'integer))))
         (cond
           ((< raw 0) raw)
           (t (+ 1 raw))))))

(def-arbitrary-instance-type (short-float :scalar t)
    (coin-flip (random #-(or allegro sbcl) most-positive-short-float
                       #+(or allegro sbcl) most-positive-single-float)
               (- (random (- #-(or allegro sbcl) most-negative-short-float
                             #+(or allegro sbcl) most-negative-single-float)))))

(def-arbitrary-instance-type (single-float :scalar t)
     (coin-flip (random most-positive-single-float)
                (- (random (- most-negative-single-float)))))

(def-arbitrary-instance-type (double-float :scalar t)
    (coin-flip (random most-positive-double-float)
                (- (random (- most-negative-double-float)))))

(def-arbitrary-instance-type (long-float :scalar t)
    (coin-flip
     (random #-(or allegro sbcl) most-positive-long-float
             #+(or allegro sbcl) most-positive-double-float)
     (- (random (- #-(or allegro sbcl) most-negative-long-float
                   #+(or allegro sbcl) most-negative-double-float)))))

(def-arbitrary-instance-type (complex :scalar t)
    (let ((typ (coin-flip (find-class 'rational)
                          #-(or clisp ecl) (find-class 'single-float) #+(or clisp ecl) 'single-float
                          #-(or clisp ecl) (find-class 'double-float) #+(or clisp ecl) 'double-float)))
      (+ (arbitrary typ) (* #C(0 1) (arbitrary typ)))))

(defvar *default-character-range* :ascii)
(def-arbitrary-instance-type (character :key ((noncontrol t)
                                              (range *default-character-range*))
                                        :scalar t)
    (let ((max-code (case range
                      (:standard 96)
                      (:ascii 128)
                      (:ext-ascii 256)
                      (t char-code-limit))))
      (when noncontrol (setf max-code (- max-code 32)))
      (let ((actual-code (random max-code)))
        (when noncontrol (setf actual-code (+ actual-code 32)))
        (code-char actual-code))))

(def-arbitrary-instance-type (vector :key ((elem t elem-supp-p)
                                           (length (mid-biased-natnum 4))))
  (compound-structure
   (when (and (not elem-supp-p) (>= *current-compound-structure-depth*
                                    *max-compound-structure-depth*))
     (setf elem 'scalar))
   (apply #'vector (loop for i from 1 upto length
                         collect (arbitrary elem)))))

(def-arbitrary-instance-type (string :key ((noncontrol t)
                                           (range *default-character-range*)))
  (let* ((str-len (mid-biased-natnum 12))
         (result (make-string str-len)))
    (loop for i from 0 upto (- str-len 1) do
      (setf (aref result i)
            (arbitrary `(character :noncontrol ,noncontrol :range ,range))))
    result))

(def-arbitrary-instance-type (list :key ((elem t elem-supp-p)
                                         (length (mid-biased-natnum 4))))
  (compound-structure
   (when (and (not elem-supp-p) (>= *current-compound-structure-depth*
                                    *max-compound-structure-depth*))
     (setf elem 'scalar))
   (loop for i from 1 upto length collect (arbitrary elem))))

(def-arbitrary-instance-type (cons :key ((car t car-supp-p)
                                         (cdr t cdr-supp-p)))
  (compound-structure
   (when (and (not car-supp-p) (>= *current-compound-structure-depth*
                                   *max-compound-structure-depth*))
     (setf car 'scalar))
   (when (and (not cdr-supp-p) (>= *current-compound-structure-depth*
                                   *max-compound-structure-depth*))
     (setf cdr 'scalar))
   (cons (arbitrary car) (arbitrary cdr))))

(defvar *max-auto-array-rank* 4)
(def-arbitrary-instance-type (array :key ((rank nil rank-supp-p)
                                          (dimens nil dimens-supp-p)
                                          (elem t elem-supp-p)))
  (compound-structure
   (when (and (not elem-supp-p) (>= *current-compound-structure-depth*
                                    *max-compound-structure-depth*))
     (setf elem 'scalar))
   (unless rank-supp-p
     (setf rank (cond (dimens-supp-p (length dimens))
                      (t (let ((use-rank (+ 1 *max-auto-array-rank*)))
                           (loop while (> use-rank *max-auto-array-rank*) do
                                 (setf use-rank (mid-biased-natnum 2 3)))
                           use-rank)))))
   (unless dimens-supp-p
     (setf dimens (loop for d from 1 to rank collect (mid-biased-natnum 4))))

   (let ((result (make-array dimens)))
     (labels ((fill-array (rev-indices ds)
                (cond
                 ((null ds) (setf (apply #'aref result (reverse rev-indices))
                              (arbitrary elem)))
                 (t (destructuring-bind (this-dimen &rest other-dimens) ds
                      (loop for d from 0 to (- this-dimen 1) do
                            (fill-array (cons d rev-indices) other-dimens)))))))
       (fill-array nil dimens)
       result))))

(def-arbitrary-instance-type (symbol :param l
                                     :key ((existing t) (gensym nil)
                                           (exported nil) (nonnull t)
                                           (package (pick-from-sequence
                                                     (list-all-packages)))))
    (when (and existing gensym)
      (error "When generating symbol, cannot activate :gensym ~
              without suppressing :existing"))

    (cond
     ((and existing exported)  (let ((cands))
                                 (do-external-symbols (s package)
                                   (push s cands))
                                 (cond
                                  (cands (pick-from-sequence cands))
                                  (nonnull (arbitrary-by-spec l
                                                              :existing t
                                                              :exported nil))
                                  (t nil))))
     (existing  (let ((cands))
                  (do-symbols (s package) (push s cands))
                  (when cands (pick-from-sequence cands))))
     (gensym (gensym))
     (t (gentemp "" package))))

(def-arbitrary-instance-type (hash-table :key ((test nil test-supp-p)
                                               (size nil size-supp-p)
                                               (key nil key-supp-p)
                                               (val nil val-supp-p)))
  (compound-structure
   (unless test-supp-p
     (setf test (pick-from-sequence '(eq eql equal equalp))))
   (unless key-supp-p
     (setf key (case test
                 ((equal equalp)
                  (cond
                   ((>= *current-compound-structure-depth*
                        *max-compound-structure-depth*)
                    'scalar)
                   (t t)))
                 (otherwise 'scalar))))
   (unless val-supp-p
     (setf val (cond
                ((>= *current-compound-structure-depth*
                     *max-compound-structure-depth*)
                 'scalar)
                (t t))))
   (unless size-supp-p
     (setf size (mid-biased-natnum 10)))
   (let ((result (make-hash-table :test test)))
     (loop for i from 1 to size do
           (setf (gethash (arbitrary key) result) (arbitrary val)))
     result)))

(defgeneric arbitrary-grounded-type (bound)
  (:documentation "Return an arbitrary type bounded by the given type.")

  (:method ((s symbol))
     (arbitrary-grounded-type (find-class s)))

  (:method ((s (eql (find-class t))))
     (coin-flip (arbitrary-grounded-type 'symbol)
                (arbitrary-grounded-type 'number)))

  (:method ((s (eql (find-class 'symbol))))  s)

  (:method ((n (eql (find-class 'number))))
     (coin-flip (arbitrary-grounded-type (find-class 'real))
                (arbitrary-grounded-type (find-class 'complex))))

  (:method ((n (eql (find-class 'real))))
     (coin-flip (arbitrary-grounded-type (find-class 'rational))
                (arbitrary-grounded-type (find-class 'float))))

  (:method ((n (eql (find-class 'rational))))
     (coin-flip (find-class 'integer) (find-class 'ratio)))

  (:method ((n (eql (find-class 'integer))))
     (coin-flip #-(or clisp ecl) (find-class 'fixnum) #+(or clisp ecl) 'fixnum
                'bignum))

  (:method ((n (eql #-(or clisp ecl) (find-class 'fixnum)
                    #+(or clisp ecl) 'fixnum)))  n)
  (:method ((n (eql 'bignum)))               n)
  (:method ((n (eql (find-class 'ratio))))   n)

  (:method ((n (eql (find-class 'float))))
     (coin-flip #-(or sbcl allegro cmu clisp ecl scl) (find-class 'short-float)
                #+(or clisp ecl scl) 'short-float

                #-(or clisp ecl) (find-class 'single-float)
                #+(or clisp ecl) 'single-float

                #-(or clisp ecl) (find-class 'double-float)
                #+(or clisp ecl) 'double-float

                #-(or allegro sbcl cmu clisp ecl lispworks) (find-class 'long-float)
                #+(or clisp ecl) 'long-float))

  #-(or allegro sbcl cmu)
  (:method ((n (eql #-(or clisp ecl scl) (find-class 'short-float)
                    #+(or clisp ecl scl) 'short-float)))
     n)

  (:method ((n (eql #-(or clisp ecl) (find-class 'single-float)
                    #+(or clisp ecl) 'single-float)))
     n)
  (:method ((n (eql #-(or clisp ecl) (find-class 'double-float)
                    #+(or clisp ecl) 'double-float)))
     n)

  #-(or allegro sbcl cmu lispworks)
  (:method ((n (eql #-(or clisp ecl) (find-class 'long-float)
                    #+(or clisp ecl) 'long-float)))
     n)

  (:method ((n (eql (find-class 'complex)))) n))

(defun low-biased-natnum (&optional (bias 1))
  (setf bias (max bias 1))
  (multiple-value-bind (result rem)
      (floor (- (/ 1 (- 1 (apply #'* (loop for i from 1 to bias collect (random 1.0))))) 1))
    (declare (ignorable rem))
    result))

(defun mid-biased-natnum (mid &optional (bias 1))
  (coin-flip (+ mid (low-biased-natnum bias))
             (let ((result (- mid (low-biased-natnum bias))))
               (cond
                 ((< result 0) mid)
                 (t result)))))

(defun arbitrary-generable-types ()
  (loop for ty being the hash-keys of +arbitrary-generable-types+ collect ty))

(defun pick-from-sequence (seq)
  (elt seq (random (length seq))))

(def-criterion (:sample (:forms &key (values nil values-supp-p) ; deprecated
                                (domains nil)
                                (where t)
                                (where-ignore nil)
                                (where-declare nil where-declare-supp-p)
                                (verify nil verify-supp-p)
                                (sample-size 100)
                                (qualifying-sample
                                 nil qualifying-sample-supp-p)
                                (max-tries nil max-tries-supp-p))
                        :ignore)
  (declare (ignore values))
  (when values-supp-p
    (warn 'nst-soft-keyarg-deprecation :old-name :values :replacement nil))

  (unless verify-supp-p
    (error 'criterion-missing-mandatory-argument
      :criterion-name :sample :required-keyarg :verify))

  (when (not max-tries-supp-p)
        (setf max-tries (* 3 sample-size)))

  (when where-ignore
    (setf where-declare-supp-p t)
    (push `(ignore ,@where-ignore) where-declare))

  (let ((names-only (loop for d in domains
                          collect (cond ((symbolp d) d) (t (car d)))))
        (qualified 0)
        (total-samples-run 0)
        (result (make-success-report)))

    (loop for sample-num from 0
          while (and (cond
                       (max-tries-supp-p (< sample-num max-tries))
                       (t t))

                     (cond
                      ;; If there's a minimum qualifying sample, try
                      ;; at least the sample size, until we have at
                      ;; least the minimum number of qualifying tries.
                      ;; We'll check for when the user has explicitly
                      ;; waived the maximum number of tries below.
                      (qualifying-sample-supp-p
                       (or (< sample-num sample-size)
                            (< qualified qualifying-sample)))

                      ;; Otherwise the "safety" of an early exit is
                      ;; off, and we keep running until we have an
                      ;; acceptably large sample
                      (t (< sample-num sample-size))))
          do
       (let* ((this-sample (generate-sample domains))
              (where-expr `(let ,(loop for name
                                       being the hash-keys of this-sample
                                       using (hash-value value)
                                       collect `(,name ',value))
                                   ,@(when where-declare-supp-p
                                       `((declare ,@where-declare)))
                                   ,where))
              (verify-expr `(let ,(loop for name
                                        being the hash-keys of this-sample
                                        using (hash-value value)
                                        collect `(,name ',value))
                                    ,verify)))
         (format-at-verbosity 4 "Running sample ~d: ~@<~{~S~^~_~}~:>~%"
            sample-num
            (loop for name being the hash-keys of this-sample
                  using (hash-value val) collect (list name val)))
         (when (or (eq where t) (eval where-expr))
           (incf qualified)
           (block verify-once
             (handler-bind-interruptable
              ((error
                #'(lambda (e)
                    (format-at-verbosity 4 "Caught ~s in :sample criterion~%" e)
                    (add-error result
                      :format (format nil
                                  "~~@<Error ~~s~~:@_for case:~{~~:@_~s ~~s~}~~:>"
                                names-only)
                      ;; ******************************
                      ;; "names-only" below is wrong
                      ;; ******************************
                      :args (list e names-only))
                    (return-from verify-once))))

               (let ((this-result (eval verify-expr)))
                (unless this-result
                  (add-failure result
                    :format (format nil
                                "~~@<Failed for case:~{~~:@_~s ~~s~}~~:>"
                              ;; ******************************
                              ;; "names-only" below is wrong
                              ;; ******************************
                              names-only)
                    :args (list names-only))))))))
       finally (setf total-samples-run sample-num))

    (add-info result
      (format nil "Tested ~d cases of ~d generated: ~
                   ~d failure~:*~p, ~d error~:*~p"
        qualified total-samples-run
             (length (check-result-failures result))
             (length (check-result-errors result))))

    result))
(defdoc:def-documentation (criterion :sample)
    (:intro (:latex "Invariants to be tested, and the domains over which they range, are specified with the \\texttt{:sample} criterion:\\indexKeyword{sample}"))
  (:callspec (&key (verify FORM)
                   (value LAMBDA-LIST)
                   (domains (:seq (NAME SPEC)))
                   (where FORM)
                   (where-ignore ((:seq NAME)))
                   (where-declare ((:seq DECLARATION)))
                   (sample-size NUMBER)
                   (qualifying-sample NUMBER)
                   (max-tries NUMBER)))
  (:params (verify
            (:latex "The the expression to be (repeatedly) evaluated, which is expected always to return a non-null value.  This is the sole required argument, although in any particular use it is unlikely to be the only argument given."))
           (domains
            (:latex "Declares the variables in the \\texttt{verify} expression which are to be given multiple randomized values.  The default value is \\texttt{nil}, denoting an empty list."))
           (value
            (:latex "A lambda list to which the values given by the argument form should be applied. The default value is \\texttt{nil}, denoting no such arguments."))
           (where
            (:latex "A condition which determines the validity of the input argument.  For example, the condition would assert that a number is positive in an application where a negative value would be known to cause a failure.  The default value is \\texttt{t}, allowing any values."))
           (where-ignore
            (:latex "List of domain variables which are not mentioned in the \\texttt{where} clause.  These names will be declared as ignored in appropriate bindings, suppressing warnings under Lisps which check for such things in interpreted expressions.  This list need not be given explicitly when no \\texttt{where} argument is given.  Similarly, the \\texttt{where-declare} argument accepts a list of declarations to be associated with the \\texttt{where} form."))
           (sample-size
            (:latex "Gives the base specification of the number of value sets which will be generated.  Two further arguments have some bearing on the number of generation attempts when the \\texttt{where} argument is non-\\texttt{t}.  The \\texttt{qualifying-sample}\\indexKeyword{qualifying-sample} argument gives the minimum acceptable size of actual tested values, not counting sets rejected via the \\texttt{where} expression.  The \\texttt{max-tries}\\indexKeyword{max-tries} argument gives the maximum number of value sets to be generated.")))
  (:details (:seq
          (:plain "Examples:")
          (:code
           "(:sample :sample-size 10
  :domains ((x (list :elem symbol)))
  :verify (equal x (reverse (reverse x))))")
          (:code
           "(:sample :domains ((x real))
  :where (> x 1)
  :verify (< (sqrt x) x)
  :sample-size 10
  :max-tries 12)"))))

(defun generate-sample (domains)
  (let ((result (make-hash-table :test 'eq)))
    (loop for spec in domains do
      (cond
       ((symbolp spec)
        (setf (gethash spec result) (arbitrary t)))

       ((listp spec)
        (destructuring-bind (spec-name &optional (spec-type t)) spec
          (setf (gethash spec-name result) (arbitrary spec-type))))

       (t (error 'not-expected-form
            :expected-form "(NAME SPEC)" :actual-value spec))))

    result))
