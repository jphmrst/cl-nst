;;; File nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2006-2009 Smart Information Flow Technologies.
;;; Written by John Maraist.
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
    :version "1.1.2"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :mnst)))
    :depends-on ((:version :closer-mop "0.55"))

    :components ((:module "core" :components
                          (;; The NST package, plus internal packages
                           ;; and documentation generation.
                           (:file "package")

                           ;; Helper functions.
                           (:file "utils"  :depends-on ("package"))

                           ;; Error declarations.
                           (:file "errors"  :depends-on ("package"))

                           ;; Flags and generic function declarations.
                           (:file "globals"  :depends-on ("errors" "utils"))

                           ;;
                           (:file "artifacts"  :depends-on ("globals"))

                           ;; Main control flow of test and group
                           ;; execution.
                           (:file "runner"  :depends-on ("artifacts"))

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

                           ;; Receiving and bookkeeping the results of
                           ;; tests.
                           (:file "status" :depends-on ("globals" "check"))

                           ;; Standard criteria declarations.
                           (:file "criteria"
                                  :depends-on ("errors"
                                               "permuter" "check" "status"))

                           ;; Interaction with NST via the REPL.
                           (:file "command"
                                  :depends-on ("globals" "status"))

                           ;; Generating JUnit-friendly XML.
                           (:file "junit"
                                  :depends-on ("status"))

                           ;; Sample-testing predicates.
                           (:file "sampling"
                                  :depends-on ("check"))))))
