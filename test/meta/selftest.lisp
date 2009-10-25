
(in-package :mnst)

(def-test-group m-a-setup-cleanup ()
  (def-test runner (--nst-group mnst-src::simple-pass
                     (---test-passes mnst-src::simple-pass mnst-src::sp)))
  (def-test verify-simple-checkpoints
      (--nst-group mnst-src::simple-checkpoints
        (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-1)
        (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-2)
        (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-3)
        (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-4)
        (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-1)
        (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-2)
        (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-3)
        (---test-errs mnst-src::simple-checkpoints mnst-src::check-error-1))))

