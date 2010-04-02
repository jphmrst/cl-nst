
(in-package :mnst-src)

(def-test-group simple-pass ()
  (def-test sp :true t))

(def-fixtures boom-fix () (x 3) (y (error "I fail")) (z 10))

(def-test-group boom-group-fixture (boom-fix)
  (def-test bf1 :true t)
  (def-test bf2 :true t)
  (def-test bf3 :true t)
  (def-test bf4 :true t))

(def-test-group boom-group-setup ()
  (:setup (error "Setup error"))
  (:cleanup t)
  (def-test bgs1 :true t)
  (def-test bgs2 :true t)
  (def-test bgs3 :true t)
  (def-test bgs4 :true t))

(def-test-group boom-group-cleanup ()
  (:setup t)
  (:cleanup (error "Setup error"))
  (def-test bgc1 :true t)
  (def-test bgc2 :true t)
  (def-test bgc3 :true t)
  (def-test bgc4 :true t))

(def-test-group boom-test-fixture ()
  (def-test bf1 :true t)
  (def-test (bf2 :fixtures (boom-fix)) :true t)
  (def-test bf3 :true t)
  (def-test bf4 :true t))

(def-test-group boom-test-setup ()
  (def-test bts1 :true t)
  (def-test (bts2 :setup (error "Setup error") :cleanup t) :true t)
  (def-test bts3 :true t)
  (def-test bts4 :true t))

(def-test-group boom-test-cleanup ()
  (def-test btc1 :true t)
  (def-test (btc2 :setup t
                  :cleanup (progn ;; (format t "YYYYYYYYYY~%")
                                  (error "Setup error")))
      :true t)
  (def-test btc3 :true t)
  (def-test btc4 :true t))

(def-test-group fail-tests ()
  (def-test ft1 :true t)
  (def-test ft2 :true nil)
  (def-test ft3 :true (null t))
  (def-test ft4 :true t))

(def-test-group miss-difftyped-err ()
  (def-test err-3 (:err :type division-by-zero) (error "Miss this"))
  ;; (def-test err-4 (:eql 1) (div-five-by 0))
  )

(def-test-group simple-checkpoints ()
  (def-test check-pass-1 :pass 'a)
  (def-test check-pass-2 (:eq 'ert) 'ert)
  (def-test check-pass-3 (:eql 3) (+ 2 1))
  (def-test check-pass-4 (:symbol ert) 'ert)
  (def-test check-fail-1 :fail 'a)
  (def-test check-fail-2 (:eql 5) (+ 2 1))
  (def-test check-fail-3 (:symbol tre) 'ert)
  (def-test check-error-1 :true (error "This throws an error")))

(defun div-five-by (x) (/ 5 x))

;; For checking permutations of an empty list.
(def-test-group permute-nil-g ()
  (def-test pass-though-no-cands-0 (:permute :pass) '())
  (def-test fail-for-no-cands-1 (:permute (:seq (:eq 'a))) '())
  (def-test fail-for-no-cands-2 (:permute (:seq (:eq 'a))) '())
  )

;; A test which fails in one obscure corner of its criterion, so that
;; the failure report has a bit of a stack to it.
(def-test-group deeper-fail-report ()
  (def-test placehold-0 :pass '())
  (def-test deep-fail-1 (:seq :pass
                              (:all :pass (:eql 3) (:symbol b)))
    '(a b))
  (def-test placehold-1 :pass 'a)
  )

(defparameter zzz 0)

(def-fixtures fixture-with-nil ()
  (c 3)
  (nil (setf zzz 10))
  (d 'asdfg))

(def-test-group use-fixture-with-nil (fixture-with-nil)
  (def-test tN1 :true (boundp 'c))
  (def-test tN2 (:eq 'asdfg) d))
