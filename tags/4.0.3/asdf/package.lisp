;;; File package.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009-2010 Smart Information Flow Technologies.
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
(in-package :common-lisp-user)

(defpackage :sift.asdf-nst
    (:documentation "Unit and regression testing for Common Lisp")
    (:nicknames :asdf-nst)
    (:use :common-lisp :asdf)
    (:export #:report-system))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(asdf::nst-test-holder) (find-package :sift.asdf-nst))
  (export '(asdf::nst-test-holder) (find-package :asdf))
  (import '(asdf::nst-test-runner) (find-package :sift.asdf-nst))
  (export '(asdf::nst-test-runner) (find-package :asdf))
  (import '(asdf::nst-testable) (find-package :sift.asdf-nst))
  (export '(asdf::nst-testable) (find-package :asdf)))



