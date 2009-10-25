
(in-package :mnst)

(def-form-criterion (--nst-run (args &rest subcriteria) expr-form)
  (declare (ignorable expr-form))
  (destructuring-bind (&key packages groups tests) args
    `(let ((results))
       (let ((nst::+results-record+ (make-hash-table :test 'eq)))
         (declare (special nst::+results-record+))
         (loop for package in ',packages do (nst:run-package package))
         (loop for group in ',groups do (nst:run-group group))
         (loop for (group test) in ',tests do (nst:run-test group test))
         (setf results nst::+results-record+))
       ,(continue-check `(:all ,@subcriteria) `(list results)))))

(def-criterion-alias (--nst-package group-name &rest subcriteria)
  `(--nst-run (:packages (,group-name)) ,@subcriteria))
(def-criterion-alias (--nst-group group-name &rest subcriteria)
  `(--nst-run (:groups (,group-name)) ,@subcriteria))
(def-criterion-alias (--nst-test group-name test-name &rest subcriteria)
  `(--nst-run (:tests ((,group-name ,test-name))) ,@subcriteria))

(def-values-criterion (---on-test (group-name test-name &rest subcriteria)
                                  (results-hash))
  "Subcriteria get a results report of type nst:check-result"
  (let ((inst (gensym)) (result (gensym)))
    `(let* ((,inst (ensure-test-instance ',group-name ',test-name))
            (,result (gethash (nst::check-group-name ,inst) results-hash)))
       ;; (format t " Result type ~s~%" (type-of ,result))
       ,(continue-check `(:all ,@subcriteria) `(list ,result)))))

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
