;;; File interfaces.lisp
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
;;; DefDoc is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.
(defpackage :defdoc-interfaces
  (:documentation "DefDoc internal organizational package - external packages")
  (:use :common-lisp :defdoc-core :defdoc-collectors :defdoc-standard-model
        :defdoc-latex :defdoc-html)
  #+allegro (:import-from excl #:named-function))

(in-package :defdoc-interfaces)

(defmacro def-bundle-package (name repackaging &body forms)
  `(defpackage ,name ,@forms
     ,@(loop for (source . names) in repackaging
         collect `(:import-from ,source ,@names))
     (:export ,@(loop for source-and-names in repackaging
                  append (cdr source-and-names)))))

(def-bundle-package :defdoc
    ((:defdoc-core

         ;; targetdef.lisp
         #:def-target-type

         ;; elementdef.lisp
         #:def-element
       #:def-bare-string-element-tag

       ;; labels.lisp
       #:def-property-label
       #:def-label-config

       ;; tag.lisp
       #:def-doc-tag

       ;; macro.lisp
       #:def-documentation

       ;; output.lisp
       #:def-output-class
       #:write-output

       ;; collectors.lisp
       #:collect-target-type
       #:collect-symbols
       #:collect-exported-symbols
       #:collect-documented-symbols
       #:collect-all-symbols
       #:collect-output
       #:collect-named-output
       #:collect-doc
       #:collect-groups-by-label)

     (#:defdoc-latex
         ;; latex.lisp
         #:*defdoc-latex-default-directory*
         #:*latex-full-package-item-header-macro*
       #:*latex-verbatim-width*
       #:latex-style
       #:write-spec-latex
       #:write-doctype-latex
       #:write-package-specs-latex
       #:write-latex-output
       #:process-latex-document)

     (#:defdoc-html #:html-style)


     (#:defdoc-standard-model
       ;; style.lisp
       #:candidate-home-packages
       #:symbol-homing-style
       #:docspec-par-latex-style
       #:docspec-fancy-header-latex-style))
  (:documentation "Structured documentation definition")
  (:nicknames :ddoc)
  (:use :common-lisp)
  #+allegro (:import-from excl #:named-function))

(def-bundle-package :defdoc-control-api
    ((:defdoc-core

         ;; globals.lisp
         #:*docstring-style*
       #:format-docspec
       #:format-docspec-element
       #:package-exports-p

       ;; storage.lisp
       #:get-doc-spec
       #:get-doc-specs

       ;; labels.lisp
       #:labeled
       #:label-value
       #:get-label-symbol-value-translation
       #:get-labeldef
       #:get-label-class
       #:doc-label
       #:doc-label-name
       #:standard-doc-label
       #:get-compiled-labeldef
       #:process-standard-labeldef-form

       ;; targetdef.lisp
       #:standard-doc-target ;; not now in coredoc
       #:get-target-type-docspecs
       #:get-doc-target-types
       #:get-target-type

       ;; spec.lisp
       #:*spec-class*
       #:doc-spec
       #:docspec-self
       #:docspec-target-type
       #:docspec-tags

       ;; elementdef.lisp
       #:*default-element-class*
       #:string-implicit-symbol-head
       #:compile-string-element

       ;; tag.lisp
       #:get-doc-tags

       ;; output.lisp
       #:anchor
       #:*output-nesting-depth*
       #:format-default-output-contents-sep
       #:format-output-contents-sep
       #:output-contents
       #:output-contents-contents
       #:get-output-unit-title
       #:get-output-unit-author
       #:get-output-unit-leader
       #:get-output-unit-trailer
       #:format-doc-content-items
       #:format-doc-content-item
       #:format-output-preitem
       #:format-output-postitem
       #:format-output-leader-material
       #:format-output-leader-docspec
       #:format-output-leader-sep
       #:format-output-leader-title
       #:format-output-trailer-material
       #:format-output-trailer-docspec

       ;; collectors.lisp
       #:explicit-doc-element
       #:grouped-output-contents)

     (:defdoc-standard-model

         ;; standard.lisp
         #:standard-doc-spec
         #:get-element-for-docspec-format
       #:docspec-descriptive
       #:docspec-intro
       #:docspec-blurb
       #:docspec-details
       #:docspec-params
       #:docspec-callspecs
       #:docspec-deprecated
       #:with-unpacked-standard-spec

       ;; callspec.lisp
       #:standard-callspec
       #:callspec-sequence-of
       #:callspec-optional
       #:callspec-keyheaded
       #:callspec-keyarg
       #:standard-callspec
       #:callspec-sequence-of
       #:callspec-optional
       #:callspec-keyheaded
       #:callspec-keyarg

       ;; elements.lisp
       #:standard-doc-element
       #:standard-plain-text
       #:text-element-text
       #:standard-paragraph-list
       #:paragraphlist-element-items
       #:standard-sequence
       #:sequence-element-items
       #:standard-code
       #:code-element-string
       #:standard-simple-list-environment
       #:list-element-specs
       #:list-element-options
       #:list-element-env-tag
       #:standard-itemize
       #:standard-enumerate)

     (:defdoc-plaintext

         ;; block.lisp
         #:bracket-with
         #:indent-with
       #:indent-by
       #:adjoin-blocks
       #:width
       #:flow

       ;; plaintext.lisp
       #:standard-docstring-style
       #:callspec-item-to-lines
       #:output-lines
       #:callspec-prefix
       #:callspec-suffix
       #:standard-docstring-style
       #:callspec-to-lines
       #:callspec-item-to-lines
       #:output-lines
       #:callspec-prefix
       #:callspec-suffix)

     (:defdoc-latex

         ;; latex.lisp
       #:standard-latex
       #:latex-element-latex
       #:package-list-latex-mixin
       #:get-latex-output-file-name
       #:full-package-latex-style-mixin
       #:package-list-overall-header
       #:package-list-group-header
       #:package-list-entry)

     (:defdoc-html

         ;; html.lisp
         #:write-html-output-index-page
         #:format-html-output-index-page-header-block
       #:format-html-output-index-page-headers))
  (:documentation "Programmatic control of document generation."))
