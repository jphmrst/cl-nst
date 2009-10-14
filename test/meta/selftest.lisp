
(in-package :mnst)

(def-test-group m-a-setup-cleanup ()
  (def-test runner (--nst-group mnst-src::simple-pass
                     (---test-passes mnst-src::simple-pass mnst-src::sp))))

