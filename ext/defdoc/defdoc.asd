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
    :version "0.0.1"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :defdoc-test)))
    :depends-on ( :closer-mop )
    :components (;; The DEFDOC package, plus internal packages
                 ;; and documentation generation.
                 (:file "package")

                 ;; Standard comment formats.
                 (:file "globals"  :depends-on ("package"))

                 ;; Standard comment formats.
                 (:file "components"  :depends-on ("package"))

                 ;; Things we give documentation to.
                 (:file "type"  :depends-on ("components"))

                 ;; Standard comment formats.
                 (:file "specs"  :depends-on ("components"))

                 ;; Generics for the output API.
                 (:file "format"  :depends-on ("package"))

                 ;; Error declarations.
                 (:file "macro"  :depends-on ("components" "format"))

                 ;; Decoding the callspec forms.
                 (:file "callspec"  :depends-on ("package"))

                 ;; Operations on blocks of lines.
                 (:file "block"  :depends-on ("package"))

                 ;; Converting specs to plain text.
                 (:file "plaintext"  :depends-on ("specs" "callspec" "block"))

                 ;; Converting specs to LaTeX.
                 (:file "latex"  :depends-on ("globals" "specs" "callspec"))

                 ;; Documentation in def-doc of def-doc.
                 (:file "auto"  :depends-on ("globals"
                                             "block" "components" "format"
                                             "latex" "macro" "plaintext"
                                             "type"))))
