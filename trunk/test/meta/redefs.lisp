
(in-package :mnst)

(def-criterion-alias (:redef)
  (let ((group-name (gentemp))
        (test-name (gentemp)))
    `(:progn
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 6) 5)))
       (run-group ',group-name)
       (eval '(def-test-group ,group-name () (def-test ,test-name (:eql 5) 5)))
       (--nst-group ,group-name (---test-passes ,group-name ,test-name)))))

(def-test-group redefs ()
  (def-test redef :redef))
