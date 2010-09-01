
(in-package :mnst)

(def-criterion-alias (:redef)
  (let ((group-name (gentemp))
        (test-name (gentemp)))
    `(:progn
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 6) 5)))
       (run-group ',group-name)
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 5) 5)))
       (--nst-group ,group-name (---test-passes ,group-name ,test-name)))))

(def-criterion-alias (:redefN first-tests second-tests)
  (let ((group-name (gentemp)))
    `(:progn
       (eval '(def-test-group ,group-name ()
                ,@(loop for name in first-tests collect `(def-test ,name (:eql 6) 5))))
       (run-group ',group-name)
       (eval '(def-test-group ,group-name ()
                ,@(loop for name in second-tests collect `(def-test ,name (:eql 5) 5))))
       (:all ,@(loop for name in second-tests
                     collect `(--nst-group ,group-name (---test-passes ,group-name ,name)))
             ,@(loop for name in first-tests
                     if (not (member name second-tests))
                     collect `(--nst-no-test-in-group ,group-name ,name))))))

(def-test-group redefs ()
  (def-test redef0 :redef)
  (def-test redef1 (:redefN (aa bb cc) (xx yy))))
