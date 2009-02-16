;;; File asdf-nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2009 Smart Information Flow Technologies.
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

(defpackage :asdf-nst-asd (:use :common-lisp :asdf))
(in-package :asdf-nst-asd)

(defsystem :asdf-nst
    :description "NST integration with ASDF"
    :version "1.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :depends-on (:nst)
    :components ((:module "asdf" :components
			  (;; The ASDF-NST package.
			   (:file "package")
		 
			   ;; An NST-testable ASDF class
			   (:file "system"  :depends-on ("package"))
		 
;;;			   ;; An ASDF system class providing a summary
;;;			   ;; of other NST-testable systems.
;;;			   (:file "summary"  :depends-on ("package"))
			   ))))
