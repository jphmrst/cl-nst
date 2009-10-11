
(in-package :mnst)

(def-form-criterion (--nst-group (group-name &rest subcriteria) expr-form)
  (declare (ignorable expr-form))
  `(let ((results))
       (let ((nst::+results-record+ (make-hash-table :test 'eq)))
         (declare (special nst::+results-record+))
         (nst:run-group ',group-name)
         (setf results nst::+results-record+))
       ,(continue-check `(:all ,@subcriteria) `(list results))))
