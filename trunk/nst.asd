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
                                        ; In the svn repository, the
                                        ; version number is what we're
                                        ; working on now.
                                        ;
                                        ; This version number and the
                                        ; number in VERSIONS.txt must
                                        ; be kept manually
                                        ; synchronized.
                                        ;
                                        ; The patch number (third
                                        ; number) should be
                                        ; incremented immediately
                                        ; after a version is shipped.
    :version "1.3.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :nst-test)))
    :depends-on ( :closer-mop ;; (:version :closer-mop "0.55")
                 )

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

                           ;;
                           (:file "pick"  :depends-on ("artifacts"))

                           ;;
                           (:file "interrupt"  :depends-on ("package"))

                           ;; Main control flow of test and group
                           ;; execution.
                           (:file "runner"  :depends-on ("interrupt"
                                                         "artifacts"
                                                         "status"))

                           ;; The def-group macro.
                           (:file "group" :depends-on ("pick"))

                           ;; Definition and expansion of check
                           ;; criteria.
                           (:file "check" :depends-on ("pick" "interrupt"))

                           ;; The def-check macro.
                           (:file "test-def" :depends-on ("check"))

                           ;; The def-fixture macro, and processing
                           ;; anonymous fixture declarations.
                           (:file "fixture"  :depends-on ("runner" "artifacts"))

                           ;; Helper functions for permuting lists.
                           (:file "permuter" :depends-on ("package"))

                           ;; Receiving and bookkeeping the results of
                           ;; tests.
                           (:file "status" :depends-on ("globals" "check"))

                           ;; Standard criteria declarations.
                           (:file "criteria"
                                  :depends-on ("errors" "interrupt"
                                               "permuter" "check" "status"))

                           ;; Interaction with NST via the REPL.
                           (:file "command" :depends-on ("globals" "status"))

                           ;; Generating JUnit-friendly XML.
                           (:file "junit" :depends-on ("status"))

                           ;; Sample-testing predicates.
                           (:file "sampling" :depends-on ("check"))

                           ;; Other packaged APIs.
                           (:file "interfaces"
                                  :depends-on ("check" "runner" "status"))))))
