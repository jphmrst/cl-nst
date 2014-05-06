;;; File interfaces.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2011 Smart Information Flow Technologies.
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
(in-package :sift.nst)

(defmacro def-bundle-package (name repackaging &body forms)
  `(defpackage ,name ,@forms
               ,@(loop for (source . names) in repackaging
                       collect `(:import-from ,source ,@names))
               (:export ,@(loop for source-and-names in repackaging
                                append (cdr source-and-names)))))

(def-bundle-package :nst-control-api
    ((:nst #:run-package #:run-group #:run-test
           #:protect-nst-config #:apply-debug-options
           #:report-multiple #:report-package #:report-group #:report-test))
  (:documentation "API-level control of test execution and reporting."))
