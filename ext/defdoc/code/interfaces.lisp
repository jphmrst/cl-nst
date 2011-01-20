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
(in-package :defdoc)

(defmacro def-bundle-package (name repackaging &body forms)
  `(defpackage ,name ,@forms
     ,@(loop for (source . names) in repackaging
         collect `(:import-from ,source ,@names))
     (:export ,@(loop for source-and-names in repackaging
                  append (cdr source-and-names)))))

(def-bundle-package :defdoc-control-api
    ((:defdoc
       ;; globals.lisp
       #:format-docspec
       #:format-docspec-element

       ;; storage.lisp
       #:get-doc-spec
       #:get-doc-specs

       ;; targetdef.lisp
       #:standard-doc-target
       #:get-target-type-docspecs
       #:get-doc-target-types
       #:get-target-type

       ;; label.lisp
       #:labeled
       #:label-value
       #:get-label-symbol-value-translation

       ;; spec.lisp
       #:standard-doc-spec
       #:docspec-descriptive
       #:docspec-intro
       #:docspec-blurb
       #:docspec-details
       #:docspec-params
       #:docspec-callspecs
       #:docspec-deprecated
       #:with-unpacked-standard-spec
       #:*spec-class*
       #:doc-spec
       #:docspec-self
       #:docspec-target-type
       #:docspec-tags

       ;; elementdef.lisp
       #:standard-doc-element
       #:*default-element-class*
       #:string-implicit-symbol-head
       #:compile-string-element

       ;; elements.lisp
       #:standard-plain-text
       #:text-element-text
       #:standard-latex
       #:latex-element-latex
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
       #:standard-enumerate

       ;; labels.lisp
       #:get-labeldef
       #:get-label-class
       #:doc-label
       #:doc-label-name
       #:standard-doc-label
       #:get-compiled-labeldef
       #:process-standard-labeldef-form

       ;; output.lisp
       #:standard-output-framework
       #:format-output-preitem
       #:format-output-postitem
       #:output-framework
       #:output-framework-name
       ;; #:get-compiled-output-framework
       #:process-standard-output-framework-form

       ;; tag.lisp
       #:get-doc-tags

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

       ;; block.lisp
       #:bracket-with
       #:indent-with
       #:indent-by
       #:adjoin-blocks
       #:width
       #:flow

       ;; plaintext.lisp
       #:standard-docstring-style
       #:callspec-to-lines
       #:callspec-item-to-lines
       #:output-lines
       #:callspec-prefix
       #:callspec-suffix
       #:standard-docstring-style
       #:callspec-to-lines
       #:callspec-item-to-lines
       #:output-lines
       #:callspec-prefix
       #:callspec-suffix

       ;; latex.lisp
       #:package-list-latex-mixin
       #:get-latex-output-file-name
       #:latex-style
       #:latex-style-adjust-spec-element
       #:full-package-latex-style-mixin
       #:package-list-overall-header
       #:package-list-group-header
       #:package-list-entry))
  (:documentation "Programmatic control of document generation."))
