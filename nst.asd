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
    :description "The NST unit/regression testing system"
    :version "1.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :mnst)))
    :components ((:module "core" :components
			  (;; The NST package, plus internal packages
			   ;; and documentation generation.
			   (:file "package")
		 
			   ;; Flags, generic function declarations,
			   ;; and helper functions.
			   (:file "globals"  :depends-on ("package"))
		 
			   ;; The def-group macro.
			   (:file "group" :depends-on ("globals"))
		 
			   ;; The def-check macro, and the general
			   ;; expansion and handling of check
			   ;; criteria.
			   (:file "check" :depends-on ("globals"))
		 
			   ;; The def-fixture macro, and processing
			   ;; anonymous fixture declarations.
			   (:file "fixture"  :depends-on ("globals"))
		 
			   ;; Helper functions for permuting lists.
			   (:file "permuter" :depends-on ("package"))
		 
			   ;; Standard criteria declarations.
			   (:file "criteria"
				  :depends-on ("permuter" "check"))
		 
			   ;; Receiving and bookkeeping the results of
			   ;; tests.
			   (:file "status"   :depends-on ("globals"))
		 
			   ;; Interaction with NST via the REPL.
			   (:file "command"
				  :depends-on ("globals" "status"))
		 
			   ;; Generating JUnit-friendly XML.
			   (:file "junit"
				  :depends-on ("status"))

;;;		 (:file "classes")
;;;		 (:file "runners")
;;;		 (:file "testforms")
;;;		 (:file "defcheck")
;;;		 (:file "format")

			   ))))
