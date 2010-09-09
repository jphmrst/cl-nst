
(in-package :mnstmeth-src)

(nst:def-test-generic for-clses)
(nst:def-test-generic overridden
    (:method-combination t))

(defclass top-cls ()
     ((tc1 :initarg :tc1 :reader tc1)
      (tc2 :initarg :tc2 :reader tc2)))
(nst:def-test-method-criterion for-clses top-cls
      (:predicate (lambda (tc) (< (tc1 tc) (tc2 tc)))))

(defclass mid-cls (top-cls)
     ((mc1 :initarg :mc1 :reader mc1)
      (mc2 :initarg :mc2 :reader mc2)))
(nst:def-test-method for-clses (o mid-cls)
  (with-slots (mc1 mc2) o
    (cond
      ((< mc1 mc2) (make-success-report))
      (t (make-failure-report :format "~d not < ~d" :args (list mc1 mc2))))))
(nst:def-test-method-criterion overridden mid-cls
  (:slots (mc1 (:eql 0))
          (mc2 (:eql 2))))

(defclass side-cls ()
     ((sc1 :initarg :sc1 :reader sc1)
      (sc2 :initarg :sc2 :reader sc2)))
(nst:def-test-method for-clses (o side-cls)
  (with-slots (sc1 sc2) o
    (cond
      ((eql sc1 sc2) (make-success-report))
      (t (make-failure-report :format "~d not eql ~d" :args (list sc1 sc2))))))

(defclass bot-cls (mid-cls side-cls)
     ((bc1 :initarg :bc1 :reader bc1)
      (bc2 :initarg :bc2 :reader bc2)))
(nst:def-test-method-criterion overridden bot-cls
  (:slots (sc1 (:eql 1))
          (sc2 (:eql 1))))

(def-test-group method-tests ()
  (def-test t-p :methods (make-instance 'top-cls :tc1 0 :tc2 2))
  (def-test m-p :methods (make-instance 'mid-cls :tc1 0 :tc2 2 :mc1 0 :mc2 2))
  (def-test s-p :methods (make-instance 'side-cls :sc1 1 :sc2 1))
  (def-test b-p :methods (make-instance 'bot-cls
                           :tc1 0 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 1))
  (def-test t-f :methods (make-instance 'top-cls :tc1 4 :tc2 2))
  (def-test m-f-t  :methods (make-instance 'mid-cls
                              :tc1 4 :tc2 2 :mc1 0 :mc2 2))
  (def-test m-f-m  :methods (make-instance 'mid-cls
                              :tc1 0 :tc2 2 :mc1 4 :mc2 2))
  (def-test m-f-mt :methods (make-instance 'mid-cls
                              :tc1 4 :tc2 2 :mc1 4 :mc2 2))
  (def-test s-f :methods (make-instance 'side-cls :sc1 1 :sc2 3))
  (def-test b-f-t :methods (make-instance 'bot-cls
                             :tc1 4 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 1))
  (def-test b-f-m :methods (make-instance 'bot-cls
                             :tc1 0 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 1))
  (def-test b-f-s :methods (make-instance 'bot-cls
                             :tc1 0 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 3))
  (def-test b-f-mt :methods (make-instance 'bot-cls
                              :tc1 4 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 1))
  (def-test b-f-ms :methods (make-instance 'bot-cls
                              :tc1 0 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 3))
  (def-test b-f-ts :methods (make-instance 'bot-cls
                              :tc1 4 :tc2 2 :mc1 0 :mc2 2 :sc1 1 :sc2 3))
  (def-test b-f-mts :methods (make-instance 'bot-cls
                               :tc1 4 :tc2 2 :mc1 4 :mc2 2 :sc1 1 :sc2 3)))

