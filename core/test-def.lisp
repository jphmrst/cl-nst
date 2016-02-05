;;; File test-def.lisp
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
(in-package :sift.nst)

(defun decode-defcheck-name-and-args (name-or-name-and-args)
  "This function unpacks the information inside the first form of a def-test
block, which can be either a single symbol naming the test, or a list whose
first element is that symbol and whose remaining elements are options."

  (cond
   ((symbolp name-or-name-and-args)
    (values name-or-name-and-args nil nil nil nil nil nil nil nil nil nil))
   ((listp name-or-name-and-args)
    (destructuring-bind (name &key (setup nil setup-supp-p)
                                   (cleanup nil cleanup-supp-p)
                                   (startup nil startup-supp-p)
                                   (finish nil finish-supp-p)
                                   (fixtures nil fixtures-supp-p)
                                   (group nil group-supp-p)
                                   (aspirational nil aspirational-supp-p)
                                   (documentation nil documentation-supp-p))
        name-or-name-and-args
      (when (and fixtures (symbolp fixtures))
        (setf fixtures (list fixtures)))
      (values name
                setup setup-supp-p cleanup cleanup-supp-p
                startup startup-supp-p finish finish-supp-p
                fixtures fixtures-supp-p
                group group-supp-p aspirational aspirational-supp-p
                documentation documentation-supp-p)))
   (t (error "~@<Expected symbol or list for def-test argument~_ ~s~:>"
             name-or-name-and-args))))

;;; -----------------------------------------------------------------

(defstruct test-record
  "Structure storing parsed NST test records.
- =group= :: The group-record structure (not the symbolic name) of the group
             containing this test."
  name group fixtures criterion forms special-fixture-names
  setup cleanup startup finish results aspirational aspirational-supp)

;; Provide debugging information about this test.
(defmethod trace-test ((gr group-record) (ts test-record))
  "Return non-nil if an item is a group record."
  (format t "Test ~s (group ~s)~%" gr ts)
  (format t " - Given criterion: ~s~%" (test-record-criterion ts))
  (format t " - Given forms: ")
  (pprint-logical-block (*standard-output* (test-record-forms ts))
    (loop for form = (pprint-pop) while form do
      (format t "~s" form)
      (pprint-exit-if-list-exhausted)
      (pprint-newline :fill)))
  (format t "~%"))

(defun test-is-aspirational (test-record)
  "Check both the test and group record to see if a test should be considered
aspirational."
  (cond
    ((test-record-aspirational-supp test-record)
     (test-record-aspirational test-record))
    (t (group-record-aspirational
        (group-record (test-record-group test-record)
                      (test-record-name test-record))))))

;;; -----------------------------------------------------------------

(defmacro def-test (name-or-name-and-args criterion &rest forms)
  "Individual unit tests are encoded with the =def-test= form:
#+begin_example
\(def-test NAME ( [ :group GROUP-NAME ]
                  [ :setup FORM ]
                  [ :cleanup FORM ]
                  [ :startup FORM ]
                  [ :finish FORM ]
                  [ :fixtures (FIXTURE FIXTURE ... FIXTURE) ]
                  [ :aspirational FLAG ]
                  [ :documentation STRING ] )
  criterion &body (:seq FORM))

\(def-test NAME criterion &body (:seq FORM))
#+end_example
The =SETUP=, =CLEANUP=, =STARTUP=, =FINISH= and =FIXTURES= are just as for
fixtures and test groups, but apply only to the one test.  The =CRITERION=
is a list or symbol specifying the properties which should hold for
the =FORM=s.

When a test is not enclosed within a group body, a group name must be
provided by the =GROUP= option.  When a test is enclosed within
a group body, the =GROUP= option is not required, but if
provided it must agree with the group name.

When there are no =SETUP=, =CLEANUP=, =STARTUP=, =FINISH= or =FIXTURES=
arguments, the =NAME= may be given without parentheses.  Likewise, any
criterion consisting of a single symbol, e.g. =(:pass)=, may be abbreviated
as just the symbol without the parentheses, e.g. =:pass=.

The =:documentation= form provides a documentation string in
the standard Lisp sense.  Since documentation strings are stored
against names, and since the same name can be used for several tests
\(so long as they are all in different packages), documentation strings
on tests may not be particularly useful.

An =aspirational= test is one which verifies some part of an API or code
contract which may not yet be implemented.  When a group is marked
aspirational, all tests within the group are taken to be aspirational as well.
At this point, there is no particular processing for aspirational tests and
groups, but we hope to implement it at some point in the future.

The =def-check= form is a deprecated synonym for =def-test=."

  ;; (declare (special *group-object-variable*))

  (handler-bind (#+sbcl (style-warning
                         (named-function def-test-style-warning-handler
                           (lambda (c)
                             (muffle-warning c)))))

    ;; Decode the name-or-name-and-args, pulling out the individual
    ;; components, and indicating which are given in this test.
    (multiple-value-bind (test-name
                          setup setup-supp-p cleanup cleanup-supp-p
                          startup startup-supp-p finish finish-supp-p
                          fixtures fixtures-supp-p group group-supp-p
                          aspirational aspirational-supp-p
                          docstring docstring-supp-p)
        (decode-defcheck-name-and-args name-or-name-and-args)
      (declare (ignore fixtures-supp-p docstring-supp-p docstring))

      `(progn
         ,@(when group-supp-p
             `((when (and (boundp '*group-name*)
                          (not (eq ',group (symbol-value '*group-name*))))
                 (error "Test :group option value ~s differs from enclosing ~s"
                        ',group (symbol-value '*group-name*)))))

         ;; The actual group name and record.
         (let* ((the-group-name (cond
                                  ((boundp '*group-name*)
                                   (symbol-value '*group-name*))
                                  (t ',group)))
                (the-group-record (cond
                                    ((boundp '*group-record*)
                                     (symbol-value '*group-record*))
                                    (t (group-record ',group))))

                ;; The internal symbol used to track the results of
                ;; this test.
                (results-name nil)

                ;; A string we'll use as the basis for generated symbols.
                (base-name-string
                 (concatenate 'string
                   (package-name (symbol-package the-group-name))
                   "-" (symbol-name the-group-name)
                   "--" (package-name (symbol-package ',test-name))
                   "-" (symbol-name ',test-name))))

           (unless the-group-name
             (error "Must specify either :group option value or enclose test in def-test-group"))
           (unless the-group-record
             (error "No record for group ~s" the-group-name))

           ;; If we have a previous record for this test, re-use the
           ;; symbol for storing results, and erase the old results.
           (multiple-value-bind (old-record old-record-p)
               (test-record the-group-name ',test-name)
             (when old-record-p
               (remhash results-name (test-record-results old-record))))

           ;; If we aren't reusing a name, make up a new one.
           (unless results-name
             (setf results-name (gensym base-name-string)))

           (setf (test-record the-group-name ',test-name)
                 (make-test-record :name ',test-name
                                   :group the-group-record
                                   :criterion ',criterion
                                   :forms ',forms
                                   :fixtures ',fixtures
                                   ;; :special-fixture-names ???????????
                                   ,@(when setup-supp-p
                                       `(:setup #'(lambda () ,setup)))
                                   ,@(when cleanup-supp-p
                                       `(:cleanup #'(lambda () ,cleanup)))
                                   ,@(when startup-supp-p
                                       `(:startup #'(lambda () ,startup)))
                                   ,@(when finish-supp-p
                                       `(:finish #'(lambda () ,finish)))
                                   :results results-name
                                   :aspirational-supp ',aspirational-supp-p
                                   :aspirational ',aspirational)))))))

;;;        ;; Expand the fixtures into the definitions we'll actually
;;;        ;; use.
;;;        (multiple-value-bind
;;;            (fixture-class-names group-anon-fixture-forms fixture-names)
;;;            (process-fixture-list fixtures)
;;;          (declare (ignorable fixture-names))
;;;
;;;          (let* ((*nst-context* nil)
;;;                 (fixture-names-special
;;;                  `(special ,@(loop for fx in fixture-class-names
;;;                                  append (bound-names fx))
;;;                            ,@(loop for fx in *group-fixture-classes*
;;;                                  append (bound-names fx)))))
;;;                                        ; The expansion of the actual
;;;                                        ; test form.
;;;            (declare (special *nst-context*))
;;;
;;;            `(block ,test-name
;;;               #+allegro (excl:record-source-file ',test-name :type :nst-test)
;;;               ,@group-anon-fixture-forms
;;;
;;;               (defclass ,name (,@fixture-class-names test-record)
;;;                 ()
;;;                 ,@(when docstring-supp-p `((:documentation ,docstring))))
;;;               #-sbcl
;;;               ,@(when docstring-supp-p
;;;                   `((setf (documentation ',test-name :nst-test) ,docstring)))
;;;
;;;               (defmethod initialize-instance :after ((tproto ,name)
;;;                                                      &key &allow-other-keys)
;;;                 ,@(when fixture-names-special
;;;                     `((declare ,fixture-names-special)))
;;;                 (flet ((set-slot (slot value)
;;;                          (setf (slot-value tproto slot) value)))
;;;                   (set-slot '%group-name ',*group-class-name*)
;;;                   (set-slot '%test-name-lookup ',test-name)
;;;                   (set-slot '%check-group-name ',name)
;;;                   (set-slot '%test-forms ',forms)
;;;                   (set-slot '%aspirational ',aspirational)
;;;                   (set-slot '%aspirational-supp ',aspirational-supp-p)
;;;                   (set-slot '%special-fixture-names ',fixture-names-special)
;;;                   (set-slot '%criterion ',criterion)
;;;                   (set-slot '%setup-form
;;;                             ,(cond
;;;                                (setup-supp-p
;;;                                 `(named-function ,(format nil
;;;                                                       "test-~a-setup-thunk"
;;;                                                     name)
;;;                                   (lambda () ,setup)))
;;;                                (t '#'no-effect)))
;;;                   (set-slot '%cleanup-form
;;;                             ,(cond
;;;                               (cleanup-supp-p
;;;                                `(named-function
;;;                                     ,(format nil "test-~a-cleanup-thunk"
;;;                                        name)
;;;                                   (lambda () ,cleanup)))
;;;                                (t '#'no-effect)))
;;;                   (set-slot '%startup-form
;;;                             ,(cond
;;;                               (startup-supp-p
;;;                                `(named-function
;;;                                     ,(format nil "test-~a-startup-thunk"
;;;                                        name)
;;;                                                  (lambda () ,startup)))
;;;                                (t '#'no-effect)))
;;;                   (set-slot '%finish-form
;;;                             ,(cond
;;;                               (finish-supp-p
;;;                                `(named-function
;;;                                     ,(format nil "test-~a-finish-thunk"
;;;                                        name)
;;;                                   (lambda () ,finish)))
;;;                                (t '#'no-effect)))))
;;;
;;;               ;; Pretty printer.
;;;               (set-pprint-dispatch ',name
;;;                 (named-function ,(format nil "pprint-test--~a" name)
;;;                   (lambda (stream object)
;;;                     (declare (ignorable object))
;;;                     (format stream
;;;                         ,(format nil "Test ~s of group ~s"
;;;                            test-name
;;;                            *group-class-name*)))))
;;;
;;;               ;; (defmethod test-name-lookup ((ts ,name)) ',test-name)
;;;
;;;               (group-add-testclassname ',*group-class-name* ',name)
;;;               (add-group-test-name-and-class ',*group-class-name* ',test-name ',name)
;;;
;;;               ;; Record the use of these names.
;;;               (record-name-use :test ',test-name ',name))))

(defmacro def-check (&rest args)
  (warn 'nst-soft-deprecation :old-name 'def-check :replacement 'def-test)
  `(def-test ,@args))

;;;(defmacro debug-check (defcheck)
;;;  "Debugging aid for def-check forms.  Provides all-caps dummy values for
;;;dynamic variables normally provided by def-test-group."
;;;  `(let ((*group-fixture-classes* '(<<GROUP-FIXTURE-CLASSES>>))
;;;         (*group-class-name* '<<GROUP-CLASS-NAME>>))
;;;     (declare (special *group-class-name* *group-fixture-classes*))
;;;     (pprint (macroexpand ',defcheck))))
