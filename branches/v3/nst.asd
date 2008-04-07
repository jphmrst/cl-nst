;;; File nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006, 2007, 2008 Smart Information Flow Technologies.
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

(defpackage :nst-asd (:use :common-lisp :asdf))
(in-package :nst-asd)

(defsystem :nst
    :serial t
    :version "0.3.0"
    ;; :depends-on (:jm-defs)
    :in-order-to ((test-op (test-op :mnst)))
    :components ((:file "package")
		 (:file "permuter")
		 (:file "globals")
		 (:file "check")
		 (:file "group")
		 (:file "fixture")
		 (:file "criteria")
		 (:file "status")
;;;		 (:file "classes")
;;;		 (:file "runners")
;;;		 (:file "testforms")
;;;		 (:file "defcheck")
;;;		 (:file "interactive")
;;;		 (:file "format")
		 ))
