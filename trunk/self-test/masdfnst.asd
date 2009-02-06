;;; File msadfnst.asd
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

(asdf:oos 'asdf:load-op :asdf-nst)
(defpackage :masdfnst-asd (:use :common-lisp :asdf :asdf-nst))
(in-package :masdfnst-asd)

(defsystem :masdfnst
    :class nst-testable
    
    :in-order-to ((test-op (load-op :masdfnst)))

    ;; :nst-package :asdf-nst-test
    ;; :nst-group (:asdf-nst-test . core-checks)
    ;; :nst-test (:asdf-nst-test core-checks pass-1)

    ;; :nst-packages (:asdf-nst-test :asdf-nst-test2)

    ;; :nst-packages (:asdf-nst-test)
    ;; :nst-groups ((:asdf-nst-test2 . :g1a))
    
    :nst-groups ((:asdf-nst-test2 . :g1))
    :nst-tests ((:asdf-nst-test2 :g1a :fix0)
		(:asdf-nst-test :core-checks :warn-1))
    
    :components ((:module "asdf" :components
			  ;; A simple test suite
			  ((:file "tests") (:file "moretests")))))
