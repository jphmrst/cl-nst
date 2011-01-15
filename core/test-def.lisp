;;; File test-def.lisp
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

(defun decode-defcheck-name-and-args (name-or-name-and-args)
  "This function unpacks the information inside the first form of a def-test
block, which can be either a single symbol naming the test, or a list whose
first element is that symbol and whose remaining elements are options."

  (cond
   ((symbolp name-or-name-and-args)
    (values name-or-name-and-args nil nil nil nil nil nil nil nil))
   ((listp name-or-name-and-args)
    (destructuring-bind (name &key (setup nil setup-supp-p)
                                   (cleanup nil cleanup-supp-p)
                                   (startup nil startup-supp-p)
                                   (finish nil finish-supp-p)
                                   (fixtures nil fixtures-supp-p)
                                   (group nil group-supp-p)
                                   (documentation nil documentation-supp-p))
        name-or-name-and-args
      (when (and fixtures (symbolp fixtures))
        (setf fixtures (list fixtures)))
      (values name
                setup setup-supp-p cleanup cleanup-supp-p
                startup startup-supp-p finish finish-supp-p
                fixtures fixtures-supp-p
                group group-supp-p
                documentation documentation-supp-p)))
   (t
    (error "~@<Expected symbol or list for def-test argument~_ ~s~:>"
           name-or-name-and-args))))

;;; -----------------------------------------------------------------

(defclass nst-test-record ()
     ((%group-name :reader group-name)
      (%test-name-lookup :reader test-name-lookup)
      (%check-group-name :reader check-group-name)
      (%test-forms :reader test-forms)
      (%special-fixture-names :reader special-fixture-names)
      (%criterion :reader test-criterion)
      (%setup-form   :reader test-setup-form)
      (%cleanup-form :reader test-cleanup-form)
      (%startup-form :reader test-startup-form)
      (%finish-form  :reader test-finish-form))
  (:documentation "Common superclass of NST test records."))

;; Provide debugging information about this test.
(defmethod trace-test ((gr nst-group-record) (ts nst-test-record))
  (format t "Test ~s (group ~s)~%" gr ts)
  ;; (format t " - Given name and args: ~s~%" (test-forms ts))
  (format t " - Given criterion: ~s~%" (test-criterion ts))
  (format t " - Given forms: ")
  (pprint-logical-block (*standard-output* (test-forms ts))
    (loop for form = (pprint-pop) while form do
      (format t "~s" (test-forms ts))
      (pprint-exit-if-list-exhausted)
      (pprint-newline :fill)))
  (format t "~%"))

(defmethod test-record-p ((r nst-test-record))
  t)

;;; -----------------------------------------------------------------

(defmacro def-test (name-or-name-and-args criterion &rest forms)
  (declare (special *group-object-variable*))

  (handler-bind (#+sbcl (style-warning
                         (named-function def-test-style-warning-handler
                           (lambda (c)
                             ;; (format t ">>>>>>>>>>>>>>>>>>>>>>>>>~%")
                             (muffle-warning c)))))

    ;; Decode the name-or-name-and-args, pulling out the individual
    ;; components, and indicating which are given in this test.
    (multiple-value-bind (test-name
                          setup setup-supp-p cleanup cleanup-supp-p
                          startup startup-supp-p finish finish-supp-p
                          fixtures fixtures-supp-p group group-supp-p
                          docstring docstring-supp-p)
        (decode-defcheck-name-and-args name-or-name-and-args)
      (declare (ignore fixtures-supp-p))
      (when (and group-supp-p
                 (boundp '*group-class-name*)
                 (not (eq group (symbol-value '*group-class-name*))))
        (error "Test :group option value ~s differs from enclosing group ~s"
               group (symbol-value '*group-class-name*)))
      (unless (or group-supp-p (boundp '*group-class-name*))
        (error
         "Must specify either :group option value or enclose test in group"))

      (let* ((*group-class-name*
              (cond
               ((boundp '*group-class-name*) (symbol-value '*group-class-name*))
               (t group)))
             (*group-fixture-classes*
              (cond
               ((boundp '*group-fixture-classes*)
                (symbol-value '*group-fixture-classes*))
               (t (group-fixture-class-names
                   (make-instance *group-class-name*)))))

             ;; The internal symbol used to track the results of this
             ;; test.
             (name nil)

             ;; IDs of previous record of running this test.
             (purge-ids nil)

             ;; A string we'll use as the basis for generated symbols.
             (base-name-string
              (concatenate 'string
                (package-name (symbol-package *group-class-name*))
                "-" (symbol-name *group-class-name*)
                "--" (package-name (symbol-package test-name))
                "-" (symbol-name test-name)))

             (group-obj-supp-p (boundp '*group-object-variable*))
             (gproto (if group-obj-supp-p
                         *group-object-variable*
                         (gensym "gproto"))))
        (declare (special *group-class-name* *group-fixture-classes*))

        ;; Find any records of running previous versions of this test.
        (loop for report being the hash-values of +results-record+
            using (hash-key id)
            do
              (when (and (eq test-name (check-result-check-name report))
                         (eq *group-class-name*
                             (check-result-group-name report)))
                (push id purge-ids)))

        ;; If we find one, reuse the symbol.
        (when (eql 1 (length purge-ids))
          (setf name (car purge-ids)))

        ;; Get rid of any previous records.  For ticket:118, this would
        ;; become setting up initial records.
        (loop for id in purge-ids do
              (remhash id +results-record+))

        ;; If we aren't reusing a name, make up a new one.
        (unless name
          (setf name (gentemp base-name-string
                              :nst-test-class-package)))

        ;; Expand the fixtures into the definitions we'll actually
        ;; use.
        (multiple-value-bind (fixture-class-names anon-fixture-forms
                              fixture-names)
            (process-fixture-list fixtures)
          (declare (ignorable fixture-names))

          (let* ((*nst-context* nil)
                 (fixture-names-special
                  `(special ,@(loop for fx in fixture-class-names
                                  append (bound-names fx))
                            ,@(loop for fx in *group-fixture-classes*
                                  append (bound-names fx)))))
                                        ; The expansion of the actual
                                        ; test form.
            (declare (special *nst-context*))

            `(block ,test-name
               #+allegro (excl:record-source-file ',test-name :type :nst-test)
               ,@anon-fixture-forms

               (defclass ,name (,@fixture-class-names nst-test-record)
                    ()
                 (:metaclass singleton-class)
                 ;; (:metaclass nst-test-record-meta)
                 ,@(when docstring-supp-p `((:documentation ,docstring))))
               #-sbcl
               ,@(when docstring-supp-p
                   `((setf (documentation ',test-name :nst-test) ,docstring)))

               (finalize-inheritance (find-class ',name))

               ;; Clear any previous stored results, since we've just
               ;; (re-)defined this check.
               #|(when (boundp '+results-record+)
                 (remhash ',suite-class-name
                          (symbol-value '+results-record+)))|#

               ;; Pretty printer.
               (set-pprint-dispatch ',name
                 (named-function ,(format nil "pprint-test--~a" name)
                   (lambda (stream object)
                     (declare (ignorable object))
                     (format stream
                         ,(format nil "Test ~s of group ~s"
                            test-name
                            *group-class-name*)))))

               ;; (defmethod test-name-lookup ((ts ,name)) ',test-name)

               (let (,@(unless group-obj-supp-p
                         `((,gproto (make-instance ',*group-class-name*))))
                     (tproto (make-instance ',name)))
                 (flet ((set-slot (slot value)
                          (setf (slot-value tproto slot) value)))
                   (set-slot '%group-name ',*group-class-name*)
                   (set-slot '%test-name-lookup ',test-name)
                   (set-slot '%check-group-name ',name)
                   (set-slot '%test-forms ',forms)
                   (set-slot '%special-fixture-names ',fixture-names-special)
                   (set-slot '%criterion ',criterion)
                   (set-slot '%setup-form
                             ,(cond
                                (setup-supp-p
                                 `(named-function ,(format nil
                                                       "test-~a-setup-thunk"
                                                     name)
                                   (lambda () ,setup)))
                                (t '#'no-effect)))
                   (set-slot '%cleanup-form
                             ,(cond
                               (cleanup-supp-p
                                `(named-function
                                     ,(format nil "test-~a-cleanup-thunk"
                                        name)
                                   (lambda () ,cleanup)))
                                (t '#'no-effect)))
                   (set-slot '%startup-form
                             ,(cond
                               (startup-supp-p
                                `(named-function
                                     ,(format nil "test-~a-startup-thunk"
                                        name)
                                                  (lambda () ,startup)))
                                (t '#'no-effect)))
                   (set-slot '%finish-form
                             ,(cond
                               (finish-supp-p
                                `(named-function
                                     ,(format nil "test-~a-finish-thunk"
                                        name)
                                   (lambda () ,finish)))
                                (t '#'no-effect))))

                 ;; Store information about this test in its group.
                 (setf (test-list ,gproto) (nconc (test-list ,gproto)
                                                 (list ',name)))
                 (setf (gethash ',test-name (test-name-lookup ,gproto))
                       tproto)

                 ;; Record the use of these names.
                 (record-name-use :test ',test-name tproto)

                 ;; Store the new artifact against the uses of its
                 ;; name in NST.
                 (note-executable ',test-name tproto)))))))))
(def-documentation (compiler-macro def-test)
    (:properties (nst-control-api primary))
    (:tags primary)
    (:intro (:latex "Individual unit tests are encoded with the \\texttt{def-test} form:"))
  (:callspec ((NAME &key (group GROUP-NAME)
                    (setup FORM)
                    (cleanup FORM)
                    (startup FORM)
                    (finish FORM)
                    (fixtures ((:seq FIXTURE)))
                    (documentation STRING) )
              criterion &body (:seq FORM))
             (NAME criterion &body (:seq FORM)))
  (:details (:latex "The \\texttt{SETUP}, \\texttt{CLEANUP}, \\texttt{STARTUP},
\\texttt{FINISH} and \\texttt{FIXTURES} are just as for fixtures and
test groups, but apply only to the one test.  The \\texttt{CRITERION}
is a list or symbol specifying the properties which should hold for
the \\texttt{FORM}s.")

         (:latex "When a test is not enclosed within a group body, a group name must be
provided by the \\texttt{GROUP} option.  When a test is enclosed within
a group body, the \\texttt{GROUP} option is not required, but if
provided it must agree with the group name.")

         (:latex "When there are no \\texttt{SETUP}, \\texttt{CLEANUP}, \\texttt{STARTUP},
\\texttt{FINISH} or \\texttt{FIXTURES} arguments, the \\texttt{NAME} may
be given without parentheses.  Likewise, any criterion consisting of a
single symbol, e.g.\\ \\texttt{(:pass)}, may be abbreviated as just the
symbol without the parentheses, e.g.\\ \\texttt{:pass}.")

         (:latex "The \\texttt{:documentation} form provides a documentation string in
the standard Lisp sense.  Since documentation strings are stored
against names, and since the same name can be used for several tests
(so long as they are all in different packages), documentation strings
on tests may not be particularly useful.")

         (:latex "The \\texttt{def-check} form is a deprecated synonym for
\\texttt{def-test}.")))

(defpackage nst-test-class-package
    (:documentation "Internal package for NST tests' class names."))

(defmacro def-check (&rest args)
  (warn 'nst-soft-deprecation :old-name 'def-check :replacement 'def-test)
  `(def-test ,@args))

(defmacro debug-check (defcheck)
  "Debugging aid for def-check forms.  Provides all-caps dummy values for
dynamic variables normally provided by def-test-group."
  `(let ((*group-fixture-classes* '(<<GROUP-FIXTURE-CLASSES>>))
         (*group-class-name* '<<GROUP-CLASS-NAME>>))
     (declare (special *group-class-name* *group-fixture-classes*))
     (pprint (macroexpand ',defcheck))))
