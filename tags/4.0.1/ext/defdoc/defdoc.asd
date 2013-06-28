;;; File defdoc.asd
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lx2esser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(defpackage :defdoc-asd (:use :common-lisp :asdf))
(in-package :defdoc-asd)

;; Since defdoc makes multiple calls to set-pprint-dispatch, we check
;; here to make sure there's a valid, mutable dispatch map in place.
;; This caused a problem in CCL 1.6, in which *print-pprint-dispatch*
;; was initialized to nil, but this patch should work for any
;; ANSI-compliant implementation.
(unless *print-pprint-dispatch*
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil)))

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
    :license "LGPL 3.latest"
    :in-order-to ((test-op (test-op :defdoc-test)))

    ;; The features here should only even include platforms which
    ;; support Closer-to-MOP, and should be a superset of the features
    ;; guarding the inclusion of defcontract in lisp/package.lisp .
    #+(or allegro sbcl clisp) :depends-on
    #+(or allegro sbcl clisp) ( :defcontract )

    :components
    ((:module "lisp" :components
              (;; The DEFDOC package, plus internal packages and
               ;; documentation generation.
               (:file "package")
               (:file "misc" :depends-on ("package"))

               (:module "core" :depends-on ("package") :components

                        ;; Global settings
                        ((:file "globals")

                         ;; Collectable objects with properties.
                         (:file "collect")

                         ;; Storage for the actual documentation
                         ;; objects.
                         (:file "storage" :depends-on ("globals" "collect"))

                         ;; Declaring different documentation targets.
                         (:file "targetdef" :depends-on ("storage"))

                         ;; Things we give documentation to.
                         (:file "targets" :depends-on ("targetdef" "globals"))

                         ;; Properties associated with labels.
                         (:file "labels")

                         ;; General model of a document spec.
                         (:file "spec" :depends-on ("globals"))

                         ;; Scheme for defining document elements.
                         (:file "elementdef" :depends-on ("globals"))

                         ;; First cut at a tagging scheme.  Will be replaced
                         ;; by the labels/values scheme below.
                         (:file "tag")

                         ;; Selecting a set of specs.
                         (:file "select")
                                        ;  :depends-on ("standard")

                         ;; The main defdoc macro.
                         (:file "macro"  :depends-on
                                ( ;; "standard"
                                 ;; the one below are to be able to
                                 ;; use defdoc in things that depend
                                 ;; on macro.
                                 "tag" "elementdef"
                                 "targets" "targetdef"))

                         ;; Generic output framework specifications.
                         (:file "output" :depends-on
                                ("labels" "targets" "collect" "spec"))

                         ;; Generic output framework specifications.
                         (:file "collectors" :depends-on ("output"))))

               (:module "standard" :depends-on ("core") :components

                        ;; Standard representation of a specification.
                        ((:file "standard")

                         ;; Standard document element definition.
                         (:file "elements")

                         ;; Decoding the callspec forms.
                         (:file "callspec")

                         ;; Style mixins
                         (:file "style")))

               (:module "plaintext" :depends-on ("standard")
                        :components

                        ;; Utility operations on blocks of lines.
                        ((:file "utils")

                         ;; Converting specs to blocks of lines.
                         (:file "lineblocks" :depends-on ("utils"))

                         ;; Converting specs to plain text.
                         (:file "plaintext" :depends-on ("lineblocks"))))

               ;; Converting specs to LaTeX.
               (:file "latex"  :depends-on ("standard" "plaintext"))

               ;; Converting specs to HTML
               (:file "html"  :depends-on ("standard"
                                           "misc" "latex" "plaintext"))

               ;; Programmatic API
               (:file "interfaces"  :depends-on ("standard"
                                                 "core"
                                                 "plaintext" "latex" "html"))

;;; Temporarily commenting this out --- load from defdoc-doc only.
;;; SBCL doesn't like this.

;;;               ;; Documentation of def-doc in def-doc.
;;;               (:file "documentation" :depends-on ("interfaces"))
               ))))
