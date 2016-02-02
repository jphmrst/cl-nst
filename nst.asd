;;; File nst.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010-2014 Smart Information Flow Technologies.
;;; Copyright (c) 2015, 2016 John Maraist
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
    :version "5.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LLGPL 3.latest"
    :in-order-to ((test-op (test-op :nst-test))
                  (cl-user::doc-op (load-op :nst)))

    ;; The features allowing _closer-mop should only ever include
    ;; platforms which support Closer-to-MOP, and should always be a
    ;; superset of the features allowing file method below.
    :depends-on (#+(or allegro sbcl clozure openmcl clisp) :closer-mop)

    :components ((:module "core" :components
                          (;; The NST package, plus internal packages
                           ;; and documentation generation.
                           (:file "package")

                           ;; Helper functions.
                           (:file "utils"  :depends-on ("package"))

                           ;; Error declarations.
                           (:file "errors"  :depends-on ("package"))

                           ;; Macro combining handler-bind and
                           ;; interrupt detection.
                           (:file "interrupt"  :depends-on ("package"))

                           ;; Helper functions for permuting lists.
                           (:file "permuter" :depends-on ("package"))

                           ;; Flags and generic function declarations.
                           (:file "globals"  :depends-on ("errors" "utils"))

                           ;; Definition and expansion of check
                           ;; criteria.
                           (:file "context" :depends-on ("package" "globals"))

                           ;;
                           (:file "artifacts"  :depends-on ("globals"))

                           ;; The def-group macro.
                           (:file "group" :depends-on ("artifacts"))

                           ;; Definition and expansion of check
                           ;; criteria.
                           (:file "check" :depends-on ("context" "interrupt"))

                           ;; The def-fixture macro and supporting
                           ;; functions.
                           (:file "fixture" :depends-on ("errors" "globals"))

                           ;; The def-check macro.
                           (:file "test-def" :depends-on
                                  ("errors" "globals" "group"))

                           ;; Receiving and bookkeeping the results of
                           ;; tests.
                           (:file "status" :depends-on
                                  ("globals" "artifacts" "check"))

                           ;; Main control flow of test and group
                           ;; execution.
                           (:file "runner" :depends-on
                                  ("interrupt" "test-def" "group" "status"))

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

;;; TODO Put JUnit back in
;;;
;;;                           ;; Generating JUnit-friendly XML.
;;;                           (:file "xml" :depends-on ("status"))
;;;
;;;                           ;; Interfacing with JUnit
;;;                           (:file "junit" :depends-on ("xml"))

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
                                  :depends-on ("check" #|"runner"|# "status"))))))
