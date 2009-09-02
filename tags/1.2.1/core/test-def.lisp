;;; File test-def.lisp
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

(defmacro def-test (name-or-name-and-args criterion &rest forms)
  "Define a single unit test.

\(def-check NAME-OR-NAME-AND-OPTIONS
     CRITERION
   FORM ... FORM)

NAME-OR-NAME-AND-OPTIONS ::= name | NAME-AND-OPTIONS

NAME-AND-OPTIONS ::= \( name [ :fixtures FORM ]
                            [ :setup FORM ] [ :cleanup FORM ] )"

  (declare (special *group-class-name* *group-fixture-classes*))
                                        ; The def-group we're within.

  ;; Decode the name-or-name-and-args, pulling out the individual
  ;; components, and indicating which are given in this test.
  (multiple-value-bind (test-name setup setup-supp-p cleanup cleanup-supp-p
                                  fixtures fixtures-supp-p)
      (decode-defcheck-name-and-args name-or-name-and-args)
    (declare (ignorable fixtures-supp-p) (special *group-class-name*))

    (let* ((name (gensym (concatenate 'string
                           (package-name (symbol-package *group-class-name*))
                           "-"
                           (symbol-name *group-class-name*)
                           "-"
                           (package-name (symbol-package test-name))
                           "--"
                           (symbol-name test-name))))
           (*nst-context* nil)
           (core-run-body
            (cond
             ((eql 1 (length forms))
              (continue-check criterion
                              `(common-lisp:multiple-value-list ,(car forms))))
             (t
              (continue-check criterion (cons 'list forms))))))
      (declare (special *nst-context*))
      (multiple-value-bind (fixture-class-names anon-fixture-forms)
          (process-fixture-list fixtures)

        `(block ,test-name
           ,@anon-fixture-forms

           (defclass ,name (,@fixture-class-names) ()
                     (:metaclass singleton-class))

           (defmethod group-name ((obj ,name)) ',*group-class-name*)
           (defmethod check-user-name ((obj ,name)) ',test-name)
           (defmethod check-group-name ((obj ,name)) ',name)

           (defmethod core-run-test ((obj ,name))
             (declare (special ,@(loop for fx in fixture-class-names
                                     append (bound-names fx))
                               ,@(loop for fx in *group-fixture-classes*
                                       append (bound-names fx))))
             (let ((*current-group* ',*group-class-name*)
                   (*current-test*  ',test-name))
               (declare (special *current-group* *current-test*))
               ,core-run-body))

           ,@(when setup-supp-p
               `((defmethod do-group-each-test-setup progn ((obj ,name))
                   ,setup)))

           ,@(when cleanup-supp-p
               `((defmethod do-group-each-test-cleanup progn ((obj ,name))
                   ,cleanup)))

           ;; Clear any previous stored results, since we've just
           ;; (re-)defined this check.
           #|(when (boundp '+results-record+)
                 (remhash ',suite-class-name
                          (symbol-value '+results-record+)))|#

           ;; Provide debugging information about this test.
           (defmethod trace-test ((gr ,*group-class-name*)
                                  (ts ,name))
             (format t "Test ~s (group ~s)~%" gr ts)
             (format t " - Given name and args: ~s~%"
               ',name-or-name-and-args)
             (format t " - Given criterion: ~s~%" ',criterion)
             (format t " - Given forms: ~@<~{~s~^ ~:_~}~:>~%" ',forms))

           ;; Pretty printer.
           (set-pprint-dispatch ',name
                   #'(lambda (stream object)
                       (declare (ignorable object))
                       (format stream
                           ,(format nil "Test ~s of group ~s"
                              test-name
                              *group-class-name*))))

           ;; Pass the test record predicate.
           (defmethod test-record-p ((obj ,name)) t)

           (defmethod test-name-lookup ((ts ,name)) ',test-name)

           ;; Store the new artifact against the uses of its
           ;; name in NST.
           (note-executable ',test-name (make-instance ',name))

           (let ((gproto (make-instance ',*group-class-name*)))
             (setf (test-list gproto)
               (nconc (test-list gproto) (list ',name))
               (gethash ',test-name (test-name-lookup gproto))
               (make-instance ',name))))))))

(defmacro def-check (&rest args)
  (warn "def-check is deprecated; use def-test instead")
  `(def-test ,@args))

(defmacro debug-check (defcheck)
  "Debugging aid for def-check forms.  Provides all-caps dummy values for
dynamic variables normally provided by def-test-group."
  `(let ((*group-fixture-classes* '(<<GROUP-FIXTURE-CLASSES>>))
         (*group-class-name* '<<GROUP-CLASS-NAME>>))
     (declare (special *group-class-name* *group-fixture-classes*))
     (pprint (macroexpand ',defcheck))))
