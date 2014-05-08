;;; File mnst-relay.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
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

(asdf:oos 'asdf:load-op :asdf-nst)
(defpackage :mnst-asd (:use :common-lisp :asdf))
(in-package :mnst-asd)

(defsystem :mnst-relay
    :class nst-test-runner
    :description
    "Does not define NST tests, but runs the ones in a different system"
    :serial t
    :depends-on (:nst :nst-selftest-utils)
    :nst-systems (:mnst))

(defmethod operation-done-p ((o test-op) (sys (eql (find-system :mnst-relay))))
  "We need to make sure that operation-done-p doesn't return its
normal value, or a test-op will be run only once."
  (values nil))
