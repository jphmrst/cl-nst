
(in-package :mnst)

(def-test-group m-a-setup-cleanup ()
  (def-test runner (--nst-group a-setup-cleanup :true) nil))
