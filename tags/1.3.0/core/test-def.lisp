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

NAME-AND-OPTIONS ::= \( name [ :fixtures FORM ] [ :group GROUP ]
                            [ :setup FORM ] [ :cleanup FORM ] )"

  ;; Decode the name-or-name-and-args, pulling out the individual
  ;; components, and indicating which are given in this test.
  (multiple-value-bind (test-name setup setup-supp-p cleanup cleanup-supp-p
                                  fixtures fixtures-supp-p group group-supp-p)
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
           (name nil)                   ; The internal symbol used to
                                        ; track the results of this
                                        ; test.
           (purge-ids nil))             ; IDs of previous record of
                                        ; running this test.
      (declare (special *group-class-name* *group-fixture-classes*))

      ;; Find any records of running previous versions of this test.
      (loop for report being the hash-values of +results-record+
            using (hash-key id)
            do
         (when (and (eq test-name (check-result-check-name report))
                    (eq *group-class-name* (check-result-group-name report)))
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
        (setf name (gensym (concatenate 'string
                             (package-name (symbol-package
                                            *group-class-name*))
                             "-"
                             (symbol-name *group-class-name*)
                             "-"
                             (package-name (symbol-package test-name))
                             "--"
                             (symbol-name test-name)))))

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
                                  append (bound-names fx))))
               (core-run-body
                (cond ((eql 1 (length forms))
                       (continue-check criterion
                         `(common-lisp:multiple-value-list
                           (locally
                             (declare ,fixture-names-special)
                             ,(car forms)))))
                      (t (continue-check criterion
                           `(locally
                              (declare ,fixture-names-special)
                              (list ,@forms)))))))
                                        ; The expansion of the actual
                                        ; test form.
          (declare (special *nst-context*))

          `(block ,test-name
             #+allegro (excl:record-source-file ',test-name :type :nst-test)
             ,@anon-fixture-forms

             (defclass ,name (,@fixture-class-names) ()
                       (:metaclass singleton-class))

             (finalize-inheritance (find-class ',name))

             (eval-when (:load-toplevel :execute)
               (let ((this-name-use (gethash ',test-name +name-use+)))
                 (unless this-name-use
                   (setf this-name-use (make-name-use)
                         (gethash ',test-name +name-use+) this-name-use))
                 (let ((tests-by-group (name-use-tests this-name-use)))
                   (setf (gethash ',test-name tests-by-group)
                         (make-instance ',name)))))

             (handler-bind (#+sbcl (style-warning
                                    #'(lambda (c)
                                        (declare (ignore c))
                                        ;;
                                        ;; Uncomment to prove that
                                        ;; SBCL does /not/ pass
                                        ;; warnings through this
                                        ;; handler.
                                        ;;
                                        ;; (format t "++++++++++ ++++++++++~%")
                                        ;;
                                        (muffle-warning))))
               (defmethod group-name ((obj ,name)) ',*group-class-name*)
               (defmethod check-user-name ((obj ,name)) ',test-name)
               (defmethod check-group-name ((obj ,name)) ',name)

               (defmethod core-run-test ((obj ,name))
                 (declare (optimize (debug 3)) #| ,fixture-names-special |# )
                 (let ((*current-group* ',*group-class-name*)
                       (*current-test*  ',test-name))
                   (declare (special *current-group* *current-test*))
                   ,core-run-body))

               ,@(when setup-supp-p
                   `((defmethod do-test-prefixture-setup progn ((obj ,name))
                       ,setup)))

               ,@(when cleanup-supp-p
                   `((defmethod do-test-afterfixture-cleanup progn ((obj ,name))
                       ,cleanup))))

             ;; Clear any previous stored results, since we've just
             ;; (re-)defined this check.
             #|(when (boundp '+results-record+)
                 (remhash ',suite-class-name
                          (symbol-value '+results-record+)))|#

             ;; Provide debugging information about this test.
             (defmethod trace-test ((gr ,*group-class-name*)
                                    (ts ,name))
               (format t "Test ~s (group ~s)~%" gr ts)
               (format t " - Given name and args: ~s~%" ',name-or-name-and-args)
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
                     (nconc (test-list gproto) (list ',name)))
               (setf (gethash ',test-name (test-name-lookup gproto))
                     (make-instance ',name)))))))))

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
