
(in-package :mnst)

(def-test-group failure-catchers ()
  (def-test group-fixture-error
      (--nst-group mnst-src::boom-group-fixture
        (---test-errs mnst-src::boom-group-fixture mnst-src::bf1)
        (---test-errs mnst-src::boom-group-fixture mnst-src::bf2)
        (---test-errs mnst-src::boom-group-fixture mnst-src::bf3)
        (---test-errs mnst-src::boom-group-fixture mnst-src::bf4)))
  (def-test group-setup-error
      (--nst-group mnst-src::boom-group-setup
        (---test-errs mnst-src::boom-group-setup mnst-src::bgs1)
        (---test-errs mnst-src::boom-group-setup mnst-src::bgs2)
        (---test-errs mnst-src::boom-group-setup mnst-src::bgs3)
        (---test-errs mnst-src::boom-group-setup mnst-src::bgs4)))
  (def-test group-cleanup-error
      (--nst-group mnst-src::boom-group-cleanup
        (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc1)
        (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc2)
        (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc3)
        (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc4)))

  (def-test test-fixture-error
      (--nst-group mnst-src::boom-test-fixture
        (---test-passes mnst-src::boom-test-fixture mnst-src::bf1)
        (---test-errs mnst-src::boom-test-fixture mnst-src::bf2)
        (---test-passes mnst-src::boom-test-fixture mnst-src::bf3)
        (---test-passes mnst-src::boom-test-fixture mnst-src::bf4)))
  (def-test test-setup-error
      (--nst-group mnst-src::boom-test-setup
        (---test-passes mnst-src::boom-test-setup mnst-src::bts1)
        (---test-errs mnst-src::boom-test-setup mnst-src::bts2)
        (---test-passes mnst-src::boom-test-setup mnst-src::bts3)
        (---test-passes mnst-src::boom-test-setup mnst-src::bts4)))
  (def-test test-cleanup-error
      (--nst-group mnst-src::boom-test-cleanup
        (---test-passes mnst-src::boom-test-cleanup mnst-src::btc1)
        (---test-errs mnst-src::boom-test-cleanup mnst-src::btc2)
        (---test-passes mnst-src::boom-test-cleanup mnst-src::btc3)
        (---test-passes mnst-src::boom-test-cleanup mnst-src::btc4))))

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

(def-test-group permuting-empty-lists ()
  (def-test verify-permute-nil-g
      (--nst-group mnst-src::permute-nil-g
        (---test-passes mnst-src::permute-nil-g
                        mnst-src::pass-though-no-cands-0)
        (---test-fails mnst-src::permute-nil-g mnst-src::fail-for-no-cands-1)
        (---test-fails mnst-src::permute-nil-g mnst-src::fail-for-no-cands-2))))

