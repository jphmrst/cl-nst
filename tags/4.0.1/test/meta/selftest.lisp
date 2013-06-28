;;; File selftest.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.

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

#-(or lispworks clozure-common-lisp)
(def-test-group dispatched-tests ())
#-(or lispworks clozure-common-lisp)
(def-test (methods-1 :group dispatched-tests)
    (--nst-group mnstmeth-src::method-tests
      (---test-passes mnstmeth-src::method-tests mnstmeth-src::t-p)
      (---test-passes mnstmeth-src::method-tests mnstmeth-src::m-p)
      (---test-passes mnstmeth-src::method-tests mnstmeth-src::s-p)
      (---test-passes mnstmeth-src::method-tests mnstmeth-src::b-p)
      (---on-test mnstmeth-src::method-tests mnstmeth-src::t-f
        (---fail-records (:apply length (:eql 1)))
        (---info-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::m-f-t
        (---fail-records (:apply length (:eql 1)))
        (---info-records (:apply length (:eql 3)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::m-f-m
        (---fail-records (:apply length (:eql 2)))
        (---info-records (:apply length (:eql 3)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::m-f-mt
        (---fail-records (:apply length (:eql 3)))
        (---info-records (:apply length (:eql 3)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::s-f
        (---fail-records (:apply length (:eql 1)))
        (---info-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-t
        (---fail-records (:apply length (:eql 1)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-m
        (---fail-records (:apply length (:eql 1)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-s
        (---fail-records (:apply length (:eql 2)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-mt
        (---fail-records (:apply length (:eql 2)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-ms
        (---fail-records (:apply length (:eql 3)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-ts
        (---fail-records (:apply length (:eql 3)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))
      (---on-test mnstmeth-src::method-tests mnstmeth-src::b-f-mts
        (---fail-records (:apply length (:eql 4)))
        (---info-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0))))))

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

(def-test-group counter-metatest ()
  (:documentation "There was a bug in which (some) tests were run twice.")
  (def-test (ct1 :setup (setf nst-meta-sources::*test-exec-counter* 0))
      (--nst-group nst-meta-sources::counter-tests
        (:true-form (eql nst-meta-sources::*test-exec-counter* 1))))
  (def-test (ct2 :setup (setf nst-meta-sources::*test-exec-counter* 0))
      (--nst-test nst-meta-sources::counter-tests
          nst-meta-sources::counter-test
        (:true-form (eql nst-meta-sources::*test-exec-counter* 1)))))

(def-test-group group-includes-tests ()
  (:documentation "These tests validate the :include-groups feature."))
(def-test (group-includes-1 :group group-includes-tests)
    (--nst-group mnst-src::base-include
      (---test-passes mnst-src::base-include mnst-src::inc1)
      (---test-passes mnst-src::base-include mnst-src::inc2)
      (---test-passes mnst-src::included-1 mnst-src::inc1-1)
      (---test-passes mnst-src::included-1 mnst-src::inc1-2)
      (---test-passes mnst-src::included-2 mnst-src::inc2-1)
      (---test-passes mnst-src::included-2 mnst-src::inc2-2)))

(def-test-group process-failure-tests ()
  (:documentation "These tests validate the :process criterion"))
(def-test (process-failure-test-1 :group process-failure-tests)
    (--nst-group mnst-src::process-failures
      (---on-test mnst-src::process-failures mnst-src::process-fail-1
        (---fail-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))
      (---on-test mnst-src::process-failures mnst-src::process-fail-2
        (---fail-records (:apply length (:eql 4)))
        (---error-records (:apply length (:eql 1)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))
      (---on-test mnst-src::process-failures mnst-src::process-fail-3
        (---fail-records (:apply length (:eql 2)))
        (---error-records (:apply length (:eql 1)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))

(def-test-group eval-selftests ()
  (:documentation "These tests validate the :eval criterion"))
(def-test (eval-selftest-1 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-1
        (---fail-records (:apply length (:eql 0)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-2 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-2
        (---fail-records (:apply length (:eql 2)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-3 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-3
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-4 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-4
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-5 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-5
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
;;;(def-test (eval-selftest-6 :group eval-selftests)
;;;    (--nst-group mnst-src::eval-tests
;;;      (---on-test mnst-src::eval-tests mnst-src::eval-6
;;;        (---fail-records (:apply length (:eql 1)))
;;;        (---error-records (:apply length (:eql 0)))
;;;        (---warning-records (:apply length (:eql 0)))
;;;        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-eql-1 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-eql-1
        (---fail-records (:apply length (:eql 0)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-eql-2 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-eql-2
        (---fail-records (:apply length (:eql 2)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-eql-3 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-eql-3
        (---fail-records (:apply length (:eql 2)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-eql-4 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-eql-4
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-not-eql-1 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-not-eql-1
        (---fail-records (:apply length (:eql 0)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-not-eql-2 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-not-eql-2
        (---fail-records (:apply length (:eql 0)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-not-eql-3 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-not-eql-3
        (---fail-records (:apply length (:eql 2)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-not-eql-4 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-not-eql-4
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-not-eql-5 :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-not-eql-5
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))

(def-test (eval-selftest-pass :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-criterion-pass
        (---fail-records (:apply length (:eql 0)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-fail :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-criterion-fail
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))
(def-test (eval-selftest-error :group eval-selftests)
    (--nst-group mnst-src::eval-tests
      (---on-test mnst-src::eval-tests mnst-src::eval-criterion-error
        (---fail-records (:apply length (:eql 1)))
        (---error-records (:apply length (:eql 0)))
        (---warning-records (:apply length (:eql 0)))
        (---info-records (:apply length (:eql 0))))))

(def-test-group aspirational-flag-selftests ()
  (:documentation "These tests validate the :aspirational flag")
  (def-test asp0a
      (--with-test mnst-src::asp-group-0 mnst-src::ag0-t1 (---aspirational)))
  (def-test asp0b
      (--with-test mnst-src::asp-group-0 mnst-src::ag0-t2 (---aspirational)))
  (def-test asp1a
      (--with-test mnst-src::asp-group-1 mnst-src::ag1-t1 (---aspirational)))
  (def-test asp1b
      (--with-test mnst-src::asp-group-1 mnst-src::ag1-t2 (---aspirational)))
  (def-test asp1c
      (--with-test mnst-src::asp-group-1 mnst-src::ag1-t3
                   (:not ---aspirational)))
  (def-test asp2a
      (--with-test mnst-src::asp-group-2 mnst-src::ag2-t1 (---aspirational)))
  (def-test asp2b
      (--with-test mnst-src::asp-group-2 mnst-src::ag2-t2
                   (:not ---aspirational)))
  (def-test asp2c
      (--with-test mnst-src::asp-group-2 mnst-src::ag2-t3
                   (:not ---aspirational)))
  (def-test asp3a
      (--with-test mnst-src::asp-group-3 mnst-src::ag3-t1
                   (:not ---aspirational)))
  (def-test asp3b
      (--with-test mnst-src::asp-group-3 mnst-src::ag3-t2 (---aspirational))))
