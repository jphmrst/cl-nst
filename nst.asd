;;; File nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010-2014 Smart Information Flow Technologies.
;;; Written by John Maraist and other contributors.
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

#+sbcl (require 'sb-aclrepl)

(defpackage :nst-asd (:use :common-lisp :asdf))
(in-package :nst-asd)

(let ((*root-dir* (make-pathname
                   :directory (pathname-directory *load-truename*))))
  (declare (special *root-dir*))
  (unless (find-system :defdoc nil)
    (push (merge-pathnames "ext/defdoc/" *root-dir*)
          *central-registry*))
  (unless (find-system :defcontract nil)
    (push (merge-pathnames "ext/defcontract/" *root-dir*)
          *central-registry*)))

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
    :version "4.0.2"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :nst-test))
                  (cl-user::doc-op (load-op :nst)))

    ;; The features allowing _closer-mop should only ever include
    ;; platforms which support Closer-to-MOP, and should always be a
    ;; superset of the features allowing file method below.
    :depends-on ( #+(or allegro sbcl clozure openmcl clisp) :closer-mop
                  :defdoc )

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
                           (:file "interrupt"  :depends-on ("package"))

                           ;; Main control flow of test and group
                           ;; execution.
                           (:file "runner"  :depends-on ("interrupt"
                                                         "artifacts"
                                                         "status"))

                           ;; The def-group macro.
                           (:file "group" :depends-on ("artifacts"))

                           ;; Definition and expansion of check
                           ;; criteria.
                           (:file "context" :depends-on ("package"))

                           ;; Definition and expansion of check
                           ;; criteria.
                           (:file "check" :depends-on ("artifacts"
                                                       "context"
                                                       "interrupt"))

                           ;; The def-check macro.
                           (:file "test-def"
                                  :depends-on ("check" "group" "runner"))

                           ;; The def-fixture macro, and processing
                           ;; anonymous fixture declarations.
                           (:file "fixture"  :depends-on ("runner" "artifacts"))

                           ;; Helper functions for permuting lists.
                           (:file "permuter" :depends-on ("package"))

                           ;; Receiving and bookkeeping the results of
                           ;; tests.
                           (:file "status" :depends-on ("globals"
                                                        ;; "context"
                                                        "check"))

                           ;; Standard criteria declarations.
                           (:file "criteria"
                                  :depends-on ("errors" "interrupt"
                                               "permuter" "check" "status"))

                           ;; Process criteria declarations.
                           (:file "process"
                                  :depends-on ("errors" "interrupt"
                                                        "check" "status"))

                           ;; Interaction with NST via the REPL.
                           #-abcl
                           (:file "command" :depends-on ("globals" "status"))

                           ;; Generating JUnit-friendly XML.
                           (:file "xml" :depends-on ("status"))

                           ;; Interfacing with JUnit
                           (:file "junit" :depends-on ("xml"))

                           ;; Sample-testing predicates.
                           (:file "sampling" :depends-on ("check"))

                           ;; Object-oriented test methods.  The
                           ;; features allowing this file should
                           ;; always be a subset of the features
                           ;; allowing :closer-mop above.
                           #+(or allegro sbcl clozure openmcl clisp)
                           (:file "method" :depends-on ("status" "check"))

                           ;; Other packaged APIs.
                           (:file "interfaces"
                                  :depends-on ("check" "runner" "status"))))))

