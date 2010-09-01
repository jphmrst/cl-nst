
(in-package :mnst)

(def-test-group failure-catchers ()
  (:documentation "These tests validate NST's ability to catch errors."))
(def-test (group-fixture-error
           :group failure-catchers
           :documentation
           "This test checks error counting on the group mnst-src::boom-group-fixture.")
    (--nst-group mnst-src::boom-group-fixture
      (---test-errs mnst-src::boom-group-fixture mnst-src::bf1)
      (---test-errs mnst-src::boom-group-fixture mnst-src::bf2)
      (---test-errs mnst-src::boom-group-fixture mnst-src::bf3)
      (---test-errs mnst-src::boom-group-fixture mnst-src::bf4)))
(def-test (group-setup-error :group failure-catchers)
    (--nst-group mnst-src::boom-group-setup
      (---test-errs mnst-src::boom-group-setup mnst-src::bgs1)
      (---test-errs mnst-src::boom-group-setup mnst-src::bgs2)
      (---test-errs mnst-src::boom-group-setup mnst-src::bgs3)
      (---test-errs mnst-src::boom-group-setup mnst-src::bgs4)))
(def-test (group-cleanup-error :group failure-catchers)
    (--nst-group mnst-src::boom-group-cleanup
      (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc1)
      (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc2)
      (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc3)
      (---test-errs mnst-src::boom-group-cleanup mnst-src::bgc4)))
(def-test (test-fixture-error :group failure-catchers)
    (--nst-group mnst-src::boom-test-fixture
      (---test-passes mnst-src::boom-test-fixture mnst-src::bf1)
      (---test-errs mnst-src::boom-test-fixture mnst-src::bf2)
      (---test-passes mnst-src::boom-test-fixture mnst-src::bf3)
      (---test-passes mnst-src::boom-test-fixture mnst-src::bf4)))
(def-test (test-setup-error :group failure-catchers)
    (--nst-group mnst-src::boom-test-setup
      (---test-passes mnst-src::boom-test-setup mnst-src::bts1)
      (---test-errs mnst-src::boom-test-setup mnst-src::bts2)
      (---test-passes mnst-src::boom-test-setup mnst-src::bts3)
      (---test-passes mnst-src::boom-test-setup mnst-src::bts4)))
(def-test (test-cleanup-error :group failure-catchers)
    (--nst-group mnst-src::boom-test-cleanup
      (---test-passes mnst-src::boom-test-cleanup mnst-src::btc1)
      (---test-errs mnst-src::boom-test-cleanup mnst-src::btc2)
      (---test-passes mnst-src::boom-test-cleanup mnst-src::btc3)
      (---test-passes mnst-src::boom-test-cleanup mnst-src::btc4)))

(def-test-group m-a-setup-cleanup ())
(def-test (runner :group m-a-setup-cleanup)
    (--nst-group mnst-src::simple-pass
      (---test-passes mnst-src::simple-pass mnst-src::sp)))
(def-test (verify-simple-checkpoints :group m-a-setup-cleanup)
    (--nst-group mnst-src::simple-checkpoints
      (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-1)
      (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-2)
      (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-3)
      (---test-passes mnst-src::simple-checkpoints mnst-src::check-pass-4)
      (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-1)
      (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-2)
      (---test-fails mnst-src::simple-checkpoints mnst-src::check-fail-3)
      (---test-errs mnst-src::simple-checkpoints mnst-src::check-error-1)))

(def-test-group permuting-empty-lists ())
(def-test (verify-permute-nil-g :group permuting-empty-lists)
    (--nst-group mnst-src::permute-nil-g
      (---test-passes mnst-src::permute-nil-g
                      mnst-src::pass-though-no-cands-0)
      (---test-fails mnst-src::permute-nil-g mnst-src::fail-for-no-cands-1)
      (---test-fails mnst-src::permute-nil-g mnst-src::fail-for-no-cands-2)
      (---test-fails mnst-src::permute-nil-g mnst-src::fail-on-empty)
      (---test-fails mnst-src::permute-nil-g mnst-src::fail-on-fewer)
      (---test-fails mnst-src::permute-nil-g mnst-src::fail-on-more)))

(def-test-group caching-fixtures-1 ())

(def-test (cfix0 :group caching-fixtures-1
                 :setup (progn
                          (setf nst-meta-sources::*nst-fc1* 0
                                nst-meta-sources::*nst-fc2* 0)
                          (nst::flush-fixture-cache
                           'nst-meta-sources::fixtures-cache-none)))
    (--nst-group (nst-meta-sources::group-fixtures-cache-none
                  nst-meta-sources::group-fixtures-cache-none)
      (---form-true (eql nst-meta-sources::*nst-fc1* 2))
      (---form-true (eql nst-meta-sources::*nst-fc2* 2))))

(def-test (cfix1 :group caching-fixtures-1
                 :setup (progn
                          (setf nst-meta-sources::*nst-fc1* 0
                                nst-meta-sources::*nst-fc2* 0)
                          (nst::flush-fixture-cache
                           'nst-meta-sources::fixtures-cache-all)))
    (--nst-group (nst-meta-sources::group-fixtures-cache-all
                  nst-meta-sources::group-fixtures-cache-all)
      (---form-true (eql nst-meta-sources::*nst-fc1* 1))
      (---form-true (eql nst-meta-sources::*nst-fc2* 1))))

(def-test (cfix2 :group caching-fixtures-1
                 :setup (progn
                          (setf nst-meta-sources::*nst-fc1* 0
                                nst-meta-sources::*nst-fc2* 0)
                          (nst::flush-fixture-cache
                           'nst-meta-sources::fixtures-cache-one)))
    (--nst-group (nst-meta-sources::group-fixtures-cache-one
                  nst-meta-sources::group-fixtures-cache-one)
      (---form-true (eql nst-meta-sources::*nst-fc1* 1))
      (---form-true (eql nst-meta-sources::*nst-fc2* 2))))

(def-test (cfix3 :group caching-fixtures-1
                 :setup (progn
                          (setf nst-meta-sources::*nst-fc1* 0
                                nst-meta-sources::*nst-fc2* 0)
                          (nst::flush-fixture-cache
                           'nst-meta-sources::fixtures-cache-override-t)))
    (--nst-group (nst-meta-sources::group-fixtures-cache-override-t
                  nst-meta-sources::group-fixtures-cache-override-t)
      (---form-true (eql nst-meta-sources::*nst-fc1* 2))
      (---form-true (eql nst-meta-sources::*nst-fc2* 1))))

(def-test-group nil-use-fixtures ())
(def-test (nil-fix :group nil-use-fixtures
                   :setup (setf nst-meta-sources::zzz 0))
    (--nst-group nst-meta-sources::use-fixture-with-nil
      (---form-true (eql nst-meta-sources::zzz 10))))

(def-test-group fixtures-export ()
  (def-test fix-exp-0  (:package-internal :mnst-src-1) 'exp-fix-0)
  (def-test fix-exp-0a (:package-internal :mnst-src-1) 'fix0a)
  (def-test fix-exp-0b (:package-internal :mnst-src-1) 'fix0b)
  (def-test fix-exp-1  (:package-exports :mnst-src-1) 'exp-fix-1)
  (def-test fix-exp-1a (:package-exports :mnst-src-1) 'fix1a)
  (def-test fix-exp-1b (:package-exports :mnst-src-1) 'fix1b)
  (def-test fix-exp-2  (:package-exports :mnst-src-1) 'exp-fix-2)
  (def-test fix-exp-2a (:package-internal :mnst-src-1) 'fix2a)
  (def-test fix-exp-2b (:package-internal :mnst-src-1) 'fix2b)
  (def-test fix-exp-3  (:package-internal :mnst-src-1) 'exp-fix-3)
  (def-test fix-exp-3a (:package-exports :mnst-src-1) 'fix3a)
  (def-test fix-exp-3b (:package-exports :mnst-src-1) 'fix3b))
