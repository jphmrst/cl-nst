
(in-package :mnst)

(defvar *debug-within-metatested* nil)

(def-criterion (--nst-run (args &rest subcriteria) ())
  (let ((results))
    (destructuring-bind (&key packages groups tests) args
      (let ((nst::+results-record+ (make-hash-table :test 'eq))
            (nst::*debug-on-error*
             (cond
              (*debug-within-metatested* nst::*debug-on-error*)
              (t nil))))
        (declare (special nst::+results-record+))
        (loop for package in packages do (run-package package))
        (loop for group in groups do (run-group group))
        (loop for (group test) in tests do (run-test group test))
        (setf results nst::+results-record+))
      ;; (format t "***** Running subcriteria~%")
      (check-subcriterion-on-value `(:all ,@subcriteria) results))))

(def-criterion-alias (--nst-package group-name &rest subcriteria)
  `(--nst-run (:packages (,group-name)) ,@subcriteria))
(def-criterion-alias (--nst-group group-name &rest subcriteria)
  (cond
    ((listp group-name)
     `(--nst-run (:groups ,group-name) ,@subcriteria))
    ((symbolp group-name)
     `(--nst-run (:groups (,group-name)) ,@subcriteria))
    (t (error "Expected a symbol or list; got ~s" group-name))))
(def-criterion-alias (--nst-test group-name test-name &rest subcriteria)
  `(--nst-run (:tests ((,group-name ,test-name))) ,@subcriteria))

(def-criterion (---on-test (group-name test-name &rest subcriteria)
                               (results-hash))
  "Subcriteria get a results report of type nst:check-result"
  (let* ((inst (ensure-test-instance group-name test-name))
         (result (gethash (nst::check-group-name inst) results-hash)))
    ;; (format t " Result type ~s~%" (type-of ,result))
    (cond
      (result
       (check-subcriterion-on-value `(:all ,@subcriteria) result))
      (t (emit-failure :format "No record of test ~s (group ~s)"
                       :args (list test-name group-name))))))

(def-criterion-alias (---test-passes group-name test-name)
  `(---on-test ,group-name ,test-name
               (:predicate (lambda (r)
                             (and (null (nst::check-result-failures r))
                                  (null (nst::check-result-errors r)))))))
(def-criterion-alias (---test-fails group-name test-name)
  `(---on-test ,group-name ,test-name
               (:all
                (:predicate (lambda (r) (null (nst::check-result-errors r))))
                (:predicate (lambda (r) (nst::check-result-failures r))))))

(def-criterion-alias (---test-errs group-name test-name)
  `(---on-test ,group-name ,test-name
               (:all
                (:predicate (lambda (r) (null (nst::check-result-failures r))))
                (:predicate (lambda (r) (nst::check-result-errors r))))))

(def-criterion-alias (---error-records subcriterion)
  `(:apply (lambda (x) (check-result-errors x)) ,subcriterion))
(def-criterion-alias (---fail-records subcriterion)
  `(:apply (lambda (x) (check-result-failures x)) ,subcriterion))
(def-criterion-alias (---warning-records subcriterion)
  `(:apply (lambda (x) (check-result-warnings x)) ,subcriterion))

(def-criterion (---form-true (bool) (result))
    (declare (ignorable result))
  (let ((result (eval bool)))
    (if result
      (emit-success)
      (emit-failure :format "Expected form evaluating to non-nil, got: ~s"
                    :args (list bool)))))