;;; File defdoc.asd
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
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

(defpackage :defdoc-asd (:use :common-lisp :asdf))
(in-package :defdoc-asd)

(defsystem :defdoc
    :description "Structured document specifiers"
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
    :version "0.0.0"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :defdoc-test)))
    :depends-on ( :closer-mop )
    :components (;; The DEFDOC package, plus internal packages
                 ;; and documentation generation.
                 (:file "package")

                 ;; Defining a comment format.
                 (:file "globals"  :depends-on ("package"))

                 ;; Defining a comment format.
                 (:file "def-spec"  :depends-on ("package" "globals"))

                 ;; Standard comment formats.
                 (:file "specs"  :depends-on ("def-spec"))

                 ;; Error declarations.
                 (:file "macro"  :depends-on ("package" "def-spec"))

                 ;; Standard comment formats.
                 (:file "plaintext"  :depends-on ("specs"))

                 ;; Standard comment formats.
                 (:file "latex"  :depends-on ("specs"))

                 ))
