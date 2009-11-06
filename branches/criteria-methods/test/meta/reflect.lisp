
(in-package :mnst)

(nst:def-criterion (--nst-run (args &rest subcriteria) ())
  (let ((results))
    (destructuring-bind (&key packages groups tests) args
      (let ((nst::+results-record+ (make-hash-table :test 'eq)))
        (declare (special nst::+results-record+))
        (loop for package in packages do (nst:run-package package))
        (loop for group in groups do (nst:run-group group))
        (loop for (group test) in tests do (nst:run-test group test))
        (setf results nst::+results-record+))
      (format t "***** Running subcriteria~%")
      (let ((nst::*debug-on-error* t))
        (nst:check-subcriterion-on-value `(:all ,@subcriteria) results)))))

(def-criterion-alias (--nst-package group-name &rest subcriteria)
  `(--nst-run (:packages (,group-name)) ,@subcriteria))
(def-criterion-alias (--nst-group group-name &rest subcriteria)
  `(--nst-run (:groups (,group-name)) ,@subcriteria))
(def-criterion-alias (--nst-test group-name test-name &rest subcriteria)
  `(--nst-run (:tests ((,group-name ,test-name))) ,@subcriteria))

(nst:def-criterion (---on-test (group-name test-name &rest subcriteria)
                               (results-hash))
  "Subcriteria get a results report of type nst:check-result"
  (let* ((inst (ensure-test-instance group-name test-name))
         (result (gethash (nst::check-group-name inst) results-hash)))
    ;; (format t " Result type ~s~%" (type-of ,result))
    (nst:check-subcriterion-on-value `(:all ,@subcriteria) result)))

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
