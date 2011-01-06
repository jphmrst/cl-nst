;;; File defdoc.asd
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
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
    :license "LGPL 2.latest"
    :in-order-to ((test-op (test-op :defdoc-test)))
    :depends-on ( :closer-mop )
    :components
    ((:module "code" :components
              (;; The DEFDOC package, plus internal packages and
               ;; documentation generation.
               (:file "package")

               ;; Global settings
               (:file "globals"  :depends-on ("package"))

               ;; Collectable objects with properties.
               (:file "collect"  :depends-on ("package"))

               ;; Storage for the actual documentation objects.
               (:file "storage"  :depends-on ("globals" "collect"))

               ;; Properties associated with labels.
               (:file "labels"  :depends-on ("package"))

               ;; Declaring different documentation targets.
               (:file "targetdef"  :depends-on ("package" "storage"))

               ;; Things we give documentation to.
               (:file "targets"  :depends-on ("targetdef" "globals"))

               ;; Standard representation of a specification
               (:file "spec"  :depends-on ("package"))

               ;; Scheme for defining document elements.
               (:file "elementdef" :depends-on ("package"))

               ;; Standard document element definition.
               (:file "elements" :depends-on ("elementdef" "package"))

               ;; First cut at a tagging scheme.  Will be replaced
               ;; by the labels/values scheme below.
               (:file "tag" :depends-on ("package"))

               ;; Selecting a set of specs.
               (:file "select" :depends-on ("spec"))

               ;; (:file "values" :depends-on ("package"))

               ;; The main defdoc macro.
               (:file "macro"  :depends-on ("spec"
                                            ;; the one below are to be
                                            ;; able to use defdoc in
                                            ;; things that depend on
                                            ;; macro.
                                            "tag" "elements" "elementdef"
                                            "targets" "targetdef"
                                            ))

               ;; Generic output framework specifications.
               (:file "output"  :depends-on ("package"
                                             "labels" "targets" "elements"
                                             "collect"))

               ;; Decoding the callspec forms.
               (:file "callspec"  :depends-on ("package"
                                               "macro"))

               ;; Operations on blocks of lines.
               (:file "block"  :depends-on ("package"
                                            "macro"))

               ;; Converting specs to plain text.
               (:file "plaintext"  :depends-on ("spec" "elements"
                                                       "callspec" "block"
                                                       "macro"))

               ;; Converting specs to LaTeX.
               (:file "latex"  :depends-on ("globals"
                                            "macro" "plaintext" "spec"
                                            "elements" "callspec" "tag"
                                            "output"))

               ;; Documentation of def-doc in def-doc.
               (:file "coredoc"  :depends-on ("globals"
                                              "macro" "storage" "targetdef"
                                              "spec" "elementdef" "elements"
                                              "tag" "macro" "callspec" "block"
                                              "plaintext" "latex"))

               ;; Programmatic API
               (:file "interfaces"  :depends-on ("storage"
                                                 "targetdef"
                                                 "spec"
                                                 "elementdef"
                                                 "elements"
                                                 "labels"
                                                 "tag"
                                                 "callspec"
                                                 "block"
                                                 "output"
                                                 "plaintext"
                                                 "latex"))))))
