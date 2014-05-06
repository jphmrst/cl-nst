;;; File methods.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2014 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;; Derived from RRT, Copyright (c) 2005 Robert Goldman.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.

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

