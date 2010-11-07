;;; File interfaces.lisp
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

       ;; spec.lisp
       #:*spec-class*
       #:doc-spec
       #:docspec-self
       #:docspec-target-type
       #:docspec-tags
       #:set-docspec-property
       #:get-docspec-property
       #:standard-doc-spec
       #:docspec-descriptive
       #:docspec-intro
       #:docspec-short
       #:docspec-full
       #:docspec-params
       #:docspec-callspecs
       #:docspec-deprecated
       #:with-unpacked-standard-spec

       ;; elementdef.lisp
       #:*default-element-class*
       #:standard-doc-element

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
       #:get-output-framework
       #:get-output-framework-class
       #:output-framework
       #:output-framework-name
       #:standard-output-framework
       #:get-compiled-output-framework
       #:process-standard-output-framework-form

       ;; tag.lisp
       #:get-doc-tags

       ;; callspec.lisp
       #:standard-callspec
       #:callspec-sequence-of
       #:callspec-optional
       #:callspec-keyheaded
       #:callspec-keyarg

       ;; block.lisp
       #:indent-by
       #:bracket-with
       #:width
       #:flow

       ;; plaintext.lisp
       #:standard-docstring-style
       #:callspec-to-lines
       #:callspec-item-to-lines
       #:output-lines
       #:callspec-prefix
       #:callspec-suffix

       ;; latex.lisp
       #:get-latex-output-file-name
       #:latex-style
       #:latex-style-adjust-spec-element
       #:full-package-latex-style-mixin
       #:package-list-overall-header
       #:package-list-group-header
       #:package-list-entry
       #:package-list-latex-mixin))
  (:documentation "Programmatic control of document generation."))
