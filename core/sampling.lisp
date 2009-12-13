;;; File sampling.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009 Smart Information Flow Technologies.
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

(defmacro compound-structure (&body forms)
  `(let ((*current-compound-structure-depth*
          (+ 1 *current-compound-structure-depth*)))
     ,@forms))

(defgeneric arbitrary (typ)
  (:documentation "Return an arbitrary element of the given type.")

  (:method ((spec cons))
     (apply #'arbitrary-by-spec (car spec) (cdr spec)))

  (:method (other)
     (arbitrary-by-spec other)))

(defgeneric arbitrary-by-spec (name &key &allow-other-keys)
  (:documentation "Return an arbitrary value according to a spec")

  (:method (unknown &rest keyword-args)
     (error "Cannot generate an arbitrary value of type: ~s~{ ~s~}"
            unknown keyword-args))

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
     (t (error "Expected symbol or list, got ~s" type-expr)))

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
               (- (random (- #-(or allegro sbcl) least-negative-short-float
                             #+(or allegro sbcl) least-negative-single-float)))))

(def-arbitrary-instance-type (single-float :scalar t)
     (coin-flip (random most-positive-single-float)
                (- (random (- least-negative-single-float)))))

(def-arbitrary-instance-type (double-float :scalar t)
    (coin-flip (random most-positive-double-float)
                (- (random (- least-negative-double-float)))))

(def-arbitrary-instance-type (long-float :scalar t)
    (coin-flip
     (random #-(or allegro sbcl) most-positive-long-float
             #+(or allegro sbcl) most-positive-double-float)
     (- (random (- #-(or allegro sbcl) least-negative-long-float
                   #+(or allegro sbcl) least-negative-double-float)))))

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
     (coin-flip #-(or sbcl allegro cmu) #-(or clisp ecl) (find-class 'short-float) #+(or clisp ecl) 'short-float
                #-(or clisp ecl) (find-class 'single-float) #+(or clisp ecl) 'single-float
                #-(or clisp ecl) (find-class 'double-float) #+(or clisp ecl) 'double-float
                #-(or allegro sbcl cmu) #-(or clisp ecl) (find-class 'long-float) #+(or clisp ecl) 'long-float))

  #-(or allegro sbcl cmu) (:method ((n (eql #-(or clisp ecl) (find-class 'short-float) #+(or clisp ecl) 'short-float))) n)
  (:method ((n (eql #-(or clisp ecl) (find-class 'single-float) #+(or clisp ecl) 'single-float))) n)
  (:method ((n (eql #-(or clisp ecl) (find-class 'double-float) #+(or clisp ecl) 'double-float))) n)
  #-(or allegro sbcl cmu) (:method ((n (eql #-(or clisp ecl) (find-class 'long-float) #+(or clisp ecl) 'long-float))) n)

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

(def-form-criterion (:sample (&key (domains nil) (where t) (values nil)
                                   (verify nil verify-supp-p)
                                   (sample-size 100)
                                   (qualifying-sample nil
                                                      qualifying-sample-supp-p)
                                   (max-tries nil max-tries-supp-p))
                             expr-list-form)

  (unless verify-supp-p
    (error "criterion :sample requires :verify argument"))

  (when (not max-tries-supp-p)
    (setf max-tries (* 3 sample-size)))

  (let* ((sample-num-var (gensym "sample-num-"))
         (qualifying-sample-var (gensym "qualifying-"))
         (total-samples-run-var (gensym "total-run-"))
         (result (gensym))

         (names-only (loop for d in domains
                           collect (cond ((symbolp d) d) (t (car d)))))

         (test-while-expr (cond

                            ;; If there's a minimum qualifying sample,
                            ;; try at least the sample size, until we
                            ;; have at least the minimum number of
                            ;; qualifying tries.  We'll check for when
                            ;; the user has explicitly waived the
                            ;; maximum number of tries below.
                            (qualifying-sample-supp-p
                             `(or (< ,sample-num-var ,sample-size)
                                  (< ,qualifying-sample-var
                                     ,qualifying-sample)))


                            ;; Otherwise the "safety" of an early exit
                            ;; is off, and we keep running until we
                            ;; have an acceptably large sample
                            (t `(< ,qualifying-sample-var ,sample-size)))))

    ;; If there's a maximum sample size given (and there is a default,
    ;; so we check the value itself so avoid the case where it's been
    ;; nulled-out), then we impose an additional condition that we
    ;; haven't exceeded that number.
    (when max-tries
      (setf test-while-expr
            `(and ,test-while-expr (< ,sample-num-var ,max-tries))))

    `(multiple-value-bind ,values ,expr-list-form
       (let ((,qualifying-sample-var 0)
             (,result (emit-success))
             ,total-samples-run-var)

         (loop for ,sample-num-var from 0
               while ,test-while-expr
               do
            (let ,(loop for domain-spec in domains
                         collect
                           (cond
                            ((symbolp domain-spec)
                             `(,domain-spec (arbitrary t)))

                            ((listp domain-spec)
                             (destructuring-bind (name &optional (spec t))
                                 domain-spec
                               `(,name (arbitrary ',spec))))

                            (t (error "Expected (NAME DOMAIN-SPEC) but got ~s"
                                      domain-spec))))
              (when ,where
                (incf ,qualifying-sample-var)
                (block verify-once
                  (handler-bind-interruptable
                      ((error
                        #'(lambda (e)
                            (format-at-verbosity 4
                                "Caught ~s in :sample criterion~%"
                              e)
                            (add-error
                                ,result
                              :format ,(format nil
                                           "~~@<Error ~~s~~:@_for case:~{~~:@_~s ~~s~}~~:>"
                                         names-only)
                              :args (list e (list ,@names-only)))
                            (return-from verify-once))))
                    (let ((this-result ,verify))
                      (unless this-result
                        (add-failure
                         ,result
                         :format ,(format nil
                                      "~~@<Failed for case:~{~~:@_~s ~~s~}~~:>"
                                    names-only)
                         :args (list ,@names-only))))))))
               finally (setf ,total-samples-run-var ,sample-num-var))

         (add-info ,result
                   (format nil "Tested ~d cases of ~d generated: ~
                                ~d failure~:*~p, ~d error~:*~p"
                             ,qualifying-sample-var ,total-samples-run-var
                             (length (check-result-failures ,result))
                             (length (check-result-errors ,result))))

         ,result))))
