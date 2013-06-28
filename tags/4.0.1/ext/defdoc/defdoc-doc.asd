;;; File defdoc-doc.asd
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

(defpackage :defdoc-doc-asd (:use :common-lisp :asdf))
(in-package :defdoc-doc-asd)

(asdf:oos 'asdf:load-op :asdf-defdoc)

(defsystem :defdoc-doc
  :description "Documentation builder for defdoc"
  :class defdoc-asdf
  :depends-on ( :defdoc :asdf-defdoc )
  :documents-system :defdoc
  :components ((:module "lisp" :components
                        ;; Documentation of def-doc in def-doc.
                        ((:file "documentation")))
               (:module "doc" :depends-on ("lisp") :components
                        (;; The NST package, plus internal packages
                         ;; and documentation generation.
                         (:file "package")

                         ;; Helper functions.
                         (:file "doc"  :depends-on ("package")))))
  :documentation-package :defdoc-doc
  :build-output ((#:defdoc-manual :rel-directory "doc/"
                                  :filename-root "html"
                                  :style #:html-style
                                  :index t :table-of-contents t)
                 (#:defdoc-manual :rel-directory "doc/"
                                  :style #:manual-style
                                  :index t :table-of-contents t)
                 (#:defdoc-manual :rel-directory "doc/"
                                  :style #:plaintext-style
                                  :index t :table-of-contents t)))
