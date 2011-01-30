;;; File package.lisp
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
(in-package :common-lisp-user)

(defpackage :defdoc-docsyms
    (:export #:manual-section #:docspecs #:outspec ; #:doc-gen
             #:control #:targets #:model
             #:label-model #:elements #:standard-model #:output-model
             #| #:plaintext |#
             #:plaintext-model #:styles #:latex #:latex-style-model
             #:deprecated))

(defpackage :defdoc-core
  (:documentation "DefDoc internal organizational package - core operations")
  (:use :common-lisp :defdoc-docsyms)
  #+allegro (:import-from excl #:named-function)
  (:export #:*docstring-style*
           ;; globals.lisp
           #-allegro #:named-function
           #:*latex-verbatim-width*
           #:format-doc
           #:format-docspec
           #:format-docspec-element
           #:package-exports-p
           #:locate-package-home

           ;; storage.lisp
           #:get-doc-spec
           #:get-doc-specs
           #:get-doc-hash-of-target-type

           ;; labels.lisp
           #:def-property-label
           #:labeled
           #:label-values
           #:label-value
           #:doc-label-name
           #:get-label-symbol-value-translation
           #:get-label-section-title
           #:get-label-section-title-supp-p
           #:get-label-section-order
           #:get-label-section-order-supp-p

           ;; targetdef.lisp
           #:standard-doc-target ;; not now in coredoc
           #:get-target-type-docspecs
           #:get-doc-target-types
           #:get-target-type
           #:def-target-type
           #:capitalized-target-name
           #:lower-case-target-name

           ;; spec.lisp
           #:*spec-class*
           #:doc-spec
           #:docspec-self
           #:docspec-target-type
           #:docspec-tags
           #:get-spec-class

           ;; elementdef.lisp
           #:docspec-element
           #:def-element
           #:canonicalize-element
           #:compile-element
           #:def-bare-string-element-tag

           ;; tag.lisp
           #:get-doc-tags
           #:tag-sort
           #:format-tag

           ;; macro.lisp
           #:def-documentation

           ;; output.lisp
           #:*output-nesting-depth*
           #:*output-leader-title-format-string*
           #:def-output-class
           #:write-output
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
           #:collect-target-type
           #:collect-symbols
           #:collect-exported-symbols
           #:collect-documented-symbols
           #:collect-all-symbols
           #:collect-output
           #:collect-named-output
           #:collect-doc
           #:explicit-doc-element
           #:collect-groups-by-label
           #:grouped-output-contents
           #:get-grouped-output-labeldef
           #:get-grouped-output-group))

;;;(defmacro defdoc-core::let-echo* (bindings &body body)
;;;  (let ((x (gensym)))
;;;    `(let* ,(loop for (name form) in bindings
;;;                collect `(,name (let ((,x ,form))
;;;                                  (format t "~a = ~s~%" ',name ,x))))
;;;       ,@body)))

(defpackage :defdoc-collectors
  (:documentation
   "DefDoc internal organizational package - output collectors")
  (:use :defdoc-docsyms :common-lisp :defdoc-core)
  #+allegro (:import-from excl #:named-function)
  (:export ))

(defpackage :defdoc-standard-model
  (:documentation "DefDoc internal organizational package - standard models")
  (:use :defdoc-docsyms :common-lisp :defdoc-core)
  #+allegro (:import-from excl #:named-function)
  (:export ;; standard.lisp
           #:compile-spec
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
           #:format-standard-docspec-callspec
           #:format-standard-docspec-literal-text
           #:format-standard-docspec-param-list
           #:format-standard-docspec-param-list-start
           #:format-standard-docspec-param-list-stop
           #:format-standard-docspec-param-list-item
           #:format-standard-docspec-param-list-item-start
           #:format-standard-docspec-param-list-item-stop

           ;; callspec.lisp
           #:standard-callspec
           #:standard-callspec-mandatory
           #:standard-callspec-optional
           #:standard-callspec-optional-supp
           #:standard-callspec-key
           #:standard-callspec-key-supp
           #:standard-callspec-body
           #:standard-callspec-body-supp
           #:macrolist-callspec
           #:callspec-one-of
           #:callspec-bag-of
           #:get-callspec-holder-items
           #:callspec-sequence-of
           #:get-callspec-sequence-of-repeated
           #:callspec-optional
           #:get-callspec-optional-option
           #:callspec-keyheaded
           #:get-callspec-keyheaded-key
           #:get-callspec-keyheaded-forms
           #:callspec-keyarg
           #:get-callspec-keyarg-key
           #:get-callspec-keyarg-arg

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
           #:standard-enumerate

           ;; style.lisp
           #:candidate-home-packages
           #:symbol-homing-style
           #:docspec-par-latex-style
           #:docspec-fancy-header-latex-style))

(defpackage :defdoc-plaintext
  (:documentation "DefDoc internal organizational package - LaTeX backend")
  (:use :defdoc-docsyms :common-lisp :defdoc-core :defdoc-standard-model)
  #+allegro (:import-from excl #:named-function)
  (:export ;; block.lisp
           #:bracket-with
           #:indent-with
           #:indent-by
           #:adjoin-blocks
           #:width
           #:flow

           ;; plaintext.lisp
           #:get-default-callspec-block-width
           #:whitespace-p
           #:check-macro
           #:plaintext-style
           #:standard-docstring-style
           #:callspec-to-lines
           #:callspec-item-to-lines
           #:output-lines
           #:callspec-prefix
           #:callspec-suffix
           #:callspec-item-to-lines
           #:output-lines
           #:callspec-prefix
           #:callspec-suffix))

(defpackage :defdoc-latex
  (:documentation "DefDoc internal organizational package - LaTeX backend")
  (:use :defdoc-docsyms :common-lisp :defdoc-core :defdoc-standard-model :defdoc-plaintext)
  #+allegro (:import-from excl #:named-function)
  (:export ;; latex.lisp
           #:standard-latex
           #:latex-element-latex
           #:*defdoc-latex-default-directory*
           #:*latex-full-package-item-header-macro*
           #:package-list-latex-mixin
           #:get-latex-output-file-name
           #:full-package-latex-style-mixin
           #:package-list-overall-header
           #:package-list-group-header
           #:package-list-entry))

(defpackage :defdoc-html
  (:documentation "DefDoc internal organizational package - HTML backend")
  (:use :defdoc-docsyms :common-lisp :defdoc-core :defdoc-standard-model
        :defdoc-plaintext :defdoc-latex)
  #+allegro (:import-from excl #:named-function)
  (:export ;; html.lisp
           #:write-html-output-index-page
           #:format-html-output-index-page-header-block
           #:format-html-output-index-page-headers))
