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
  (:use :common-lisp :defcontract
        :defdoc-core :defdoc-collectors :defdoc-standard-model
        :defdoc-plaintext :defdoc-latex :defdoc-html)
  #+allegro (:import-from excl #:named-function))

(in-package :defdoc-interfaces)

(defmacro repackage-symbols (forms &body specs)
  (let ((sources (make-hash-table :test 'eq))
        (assignments (make-hash-table :test 'eq))
        (imports (make-hash-table :test 'eq))
        (other-forms (make-hash-table :test 'eq)))

    (loop for (target-package . package-forms) in forms do
      (setf (gethash target-package other-forms) package-forms))

    (loop for (source-package . repackagings) in specs do
      (loop for (target-package . symbols) in repackagings do
        (push `(,source-package ,@symbols) (gethash target-package imports))
        (loop for sym in symbols do
          (push sym (gethash target-package assignments))
          (push (intern (symbol-name sym) (find-package source-package))
                (gethash source-package sources)))))

    (loop for p being the hash-keys of sources using (hash-value syms) do
      (loop for sym in syms do
        (multiple-value-bind (symbol-echo status)
            (find-symbol (symbol-name sym) (find-package p))
          (cond
            (status
             (unless (eq status :external)
               (warn "Symbol ~a::~a repackaged but not exported"
                     (package-name p) symbol-echo))
             )
            (t (warn "Symbol ~a repackaged but not in package ~a"
                     symbol-echo (package-name p))))))
      (do-external-symbols (s p)
        (unless (member s (gethash p sources))
          (warn "Symbol ~a:~a not repackaged" (package-name p) s))))

    `(progn ,@(loop for new-package being the hash-keys of imports
                    using (hash-value sources)
                    if (not (eq new-package 'ignore))
                    collect
                    `(defpackage ,new-package
                         ,@(loop for source in sources
                                 collect `(:import-from ,@source))
                         (:export ,@(loop for source in sources
                                          append (cdr source)))
                         ,@(gethash new-package other-forms))))))

;;;
(repackage-symbols ((:defdoc
                        (:documentation "Structured documentation definition")
                        (:nicknames :ddoc)
                        (:use :common-lisp)
                        #+allegro (:import-from excl #:named-function))
                    (:defdoc-control-api
                        (:documentation
                         "Programmatic control of document generation.")))
  (:defdoc-core
      (:defdoc
          ;; targetdef.lisp
          #:def-target-type
          ;; elementdef.lisp
          #:def-element #:def-bare-string-element-tag
          ;; labels.lisp
          #:def-property-label #:def-label-config
          ;; tag.lisp
          #:def-doc-tag
          ;; macro.lisp
          #:def-documentation #:ensure-api-documentation
          ;; output.lisp
          #:def-output-class #:write-output #:get-filename-extension
          ;; collectors.lisp
          #:collect-target-type #:collect-symbols #:collect-exported-symbols
          #:collect-documented-symbols #:collect-all-symbols #:collect-output
          #:collect-named-output #:collect-doc #:collect-groups-by-label)
      (:defdoc-control-api
          ;; Last symbols --- not sorted by file
          #:get-grouped-output-labeldef #:compile-element #:get-spec-class #:element-type-p #:format-doc #:format-tag #:tag-sort #:get-label-section-title-supp-p #:*output-leader-title-format-string* #:get-grouped-output-group #:get-label-section-order-supp-p #:get-label-section-title #:get-label-section-order #:get-doc-hash-of-target-type #:label-values #:canonicalize-element
          ;; globals.lisp
          #:*docstring-style* #:format-docspec #:format-docspec-element
          #:package-exports-p #:locate-package-home
          ;; storage.lisp
          #:get-doc-spec #:get-doc-specs
          ;; labels.lisp
          #:labeled #:label-value #:get-label-symbol-value-translation
          #:get-labeldef #:get-label-class #:doc-label #:doc-label-name
          #:standard-doc-label #:get-compiled-labeldef
          #:process-standard-labeldef-form
          ;; targetdef.lisp
          #:standard-doc-target ;; not now in coredoc
          #:get-target-type-docspecs #:get-doc-target-types #:get-target-type
          #:capitalized-target-name #:lower-case-target-name
          ;; spec.lisp
          #:*spec-class* #:doc-spec #:docspec-self #:docspec-target-type
          #:docspec-tags
          ;; elementdef.lisp
          #:docspec-element #:*default-element-class*
          #:string-implicit-symbol-head #:compile-string-element
          ;; tag.lisp
          #:get-doc-tags
          ;; output.lisp
          #:*output-nesting-depth*
          #:format-default-output-contents-sep #:format-output-contents-sep
          #:output-contents #:output-contents-contents #:get-output-unit-title
          #:get-output-unit-author #:get-output-unit-leader
          #:get-output-unit-trailer
          #:format-doc-content-items #:format-doc-content-item
          #:format-output-preitem #:format-output-postitem
          #:format-output-leader-material #:format-output-leader-docspec
          #:format-output-leader-sep #:format-output-leader-title
          #:format-output-trailer-material #:format-output-trailer-docspec
          ;; collectors.lisp
          #:explicit-doc-element #:grouped-output-contents))
  (:defdoc-standard-model
      (:defdoc
          ;; style.lisp
          #:candidate-home-packages #:symbol-homing-style
          #:docspec-par-latex-style #:docspec-fancy-header-latex-style)
      (:defdoc-control-api
          ;; standard.lisp
          #:standard-doc-spec #:get-element-for-docspec-format
          #:docspec-descriptive #:docspec-intro #:docspec-blurb
          #:docspec-details #:docspec-params #:docspec-callspecs
          #:docspec-deprecated #:with-unpacked-standard-spec
          #:format-standard-docspec-literal-text
          #:format-standard-docspec-param-list-start
          #:format-standard-docspec-param-list #:compile-spec
          #:format-standard-docspec-param-list-stop
          #:format-standard-docspec-param-list-item
          #:format-standard-docspec-param-list-item-stop
          #:format-standard-docspec-param-list-item-start
          ;; callspec.lisp
          #:standard-callspec #:callspec-sequence-of #:callspec-optional
          #:callspec-keyheaded #:callspec-keyarg #:standard-callspec
          #:callspec-sequence-of #:callspec-optional #:callspec-keyheaded
          #:callspec-keyarg #:callspec-bag-of #:callspec-one-of
          #:get-callspec-keyarg-key #:standard-callspec-body-supp
          #:get-callspec-keyheaded-forms #:macrolist-callspec
          #:standard-callspec-body #:get-callspec-sequence-of-repeated
          #:standard-callspec-optional-supp #:standard-callspec-key-supp
          #:get-callspec-keyheaded-key #:format-standard-docspec-callspec
          #:get-callspec-optional-option #:get-callspec-keyarg-arg
          #:standard-callspec-optional #:standard-callspec-mandatory
          #:standard-callspec-key #:get-callspec-holder-items
          ;; elements.lisp
          #:standard-doc-element #:standard-plain-text #:text-element-text
          #:standard-paragraph-list #:paragraphlist-element-items
          #:standard-sequence #:sequence-element-items #:standard-code
          #:code-element-string #:standard-simple-list-environment
          #:list-element-specs #:list-element-options #:list-element-env-tag
          #:standard-itemize #:standard-enumerate
          #:standard-elements-style-coverage))
  (:defdoc-plaintext
      (:defdoc
          ;; plaintext.lisp
          #:plaintext-style)
      (:defdoc-control-api
          ;; block.lisp
          #:bracket-with #:indent-with #:indent-by #:adjoin-blocks
          #:width #:flow
          ;; plaintext.lisp
          #:standard-docstring-style #:callspec-item-to-lines #:output-lines
          #:callspec-prefix #:callspec-suffix #:standard-docstring-style
          #:callspec-to-lines #:callspec-item-to-lines #:output-lines
          #:callspec-prefix #:callspec-suffix #:whitespace-p
          #:get-default-callspec-block-width))
  (:defdoc-latex
      (:defdoc
          #:*defdoc-latex-default-directory*
          #:*latex-full-package-item-header-macro*
          #:*latex-verbatim-width* #:latex-style #:write-spec-latex
          #:write-doctype-latex #:write-package-specs-latex
          #:write-latex-output #:process-latex-document
          #:full-package-latex-style-mixin)
      (:defdoc-control-api
          ;; latex.lisp
          #:standard-latex #:latex-element-latex #:package-list-latex-mixin
          #:get-latex-output-file-name #:full-package-latex-style-mixin
          #:package-list-overall-header #:package-list-group-header
          #:package-list-entry #:get-latex-output-file-name
          #:package-list-overall-header
          #:*latex-full-package-item-header-macro*
          #:*defdoc-latex-default-directory*))
  (:defdoc-html
      (:defdoc #:html-style)
      (:defdoc-control-api
          #:with-div-wrapper #:get-content-link-filename
          #:traverse-and-write-output-pages
          #:format-html-output-index-page-body #:with-span-wrapper
          #:html-free-paragraph-docspec-element
          #:format-html-output-index-page-header-block
          #:get-html-disambiguator #:format-content-anchor
          #:write-html-output-index-page
          #:format-html-output-index-page-headers #:format-content-link)))

(apply-contract standard-elements-style-coverage :style 'plaintext-style)
