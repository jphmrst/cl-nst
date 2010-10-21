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
    :version "0.0.2"
    :author "John Maraist <lisper@maraist.org>"
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :defdoc-test)))
    :depends-on ( :closer-mop )
    :components (;; The DEFDOC package, plus internal packages
                 ;; and documentation generation.
                 (:file "package")

                 ;; Global settings
                 (:file "globals"  :depends-on ("package"))

                 ;; Generics for the output API.
                 (:file "format"  :depends-on ("package"))

                 ;; Storage for the actual documentation objects.
                 (:file "storage"  :depends-on ("package"))

                 ;; Declaring different documentation targets.
                 (:file "targetdef"  :depends-on ("package" "storage"))

                 ;; Things we give documentation to.
                 (:file "targets"  :depends-on ("targetdef" "globals" "format"))

                 ;; Standard representation of a specification
                 (:file "spec"  :depends-on ("package"))

                 ;; Scheme for defining document elements.
                 (:file "elementdef" :depends-on ("package"))

                 ;; Standard document element definition.
                 (:file "elements" :depends-on ("elementdef" "package"))

;;; Add tags back in later (tag.lisp copied in)
                 ;; Standard comment formats.
                 (:file "tag"  :depends-on ("package"))

                 ;; The main defdoc macro.
                 (:file "macro"  :depends-on ("spec"))

                 ;; Decoding the callspec forms.
                 (:file "callspec"  :depends-on ("package"))

                 ;; Operations on blocks of lines.
                 (:file "block"  :depends-on ("package"))

                 ;; Converting specs to plain text.
                 (:file "plaintext"  :depends-on ("spec" "elements"
                                                  "callspec" "block"))

                 ;; Converting specs to LaTeX.
                 (:file "latex"  :depends-on ("globals" "plaintext" "spec"
                                              "elements" "callspec" "tag"))

;;;                 ;; Documentation in def-doc of def-doc.
;;;                 (:file "auto"  :depends-on ("globals"
;;;                                             "block" "components" "format"
;;;                                             "latex" "macro" "plaintext"
;;;                                             "type"))
                 ))
