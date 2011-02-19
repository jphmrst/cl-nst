;;; File doc.lisp
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
(in-package :defdoc-doc)

(def-bare-string-element-tag :latex :package :defdoc-doc)

(def-documentation (type asdf-defdoc::defdoc-asdf)
  (:intro (:latex "The \\texttt{defdoc-asdf} class gives an ASDF system the effect that, when loaded, it should generate system documentation."))
  (:details (:seq "For example:"
                  (:code "(defsystem :defdoc-doc
  :description \"Documentation builder for defdoc\"
  :class defdoc-asdf
  :depends-on ( :defdoc :asdf-defdoc )
  :documents-system :defdoc
  :components ( ... )
  :documentation-package :defdoc-doc
  :build-output ((#:defdoc-manual
                     :rel-directory \"doc/\"
                     :style #:manual-style
                     :index t :table-of-contents t)))")
                  (:latex "The \\texttt{defdoc-asdf} class allows three new forms in the body of a system:")
                  (:itemize ()
                    (:seq (:latex "The \\texttt{:documents-system} forms names the ASDF system which this system documents."))
                    (:seq (:latex "The \\texttt{:documentation-package} names the package in which the symbols used in the \\texttt{:build-output} forms actually live.  Since this is an ASDF system definition, we cannot assume that this package will be loaded before the system definition."))
                    (:seq (:latex "The \\texttt{:build-output} form specifies the output units to be generated for this system.  Each element of this list is either a symbol naming an output unit, or is a list headed by such a symbol with the remaining elements taken as keyword arguments:")
                          (:itemize ()
                            (:latex "The \\texttt{:package} argument overrides the \\texttt{documentation-package} form for this output unit.")
                            (:latex "The \\texttt{:style} argument names the style class which should be instantiated for this output unit.")
                            (:latex "The \\texttt{:rel-directory} and \\texttt{:abs-directory} arguments specify the directory into which the documentation should be written.  DefDoc takes the \\texttt{:rel-directory} argument to be relative to the directory holding the ASDF system definition, and the \\texttt{:abs-directory} argument to be an absolute directory reference.  At most one of these keyword arguments should be given.")
                            (:latex "The \\texttt{:filename} argument should be astring naming the root of the file to be written.  If omitted, the name of the symbol ")
                            (:latex "The \\texttt{:index} argument, if non-nil, indicates that output should include an index if the style supports it.")
                            (:latex "The \\texttt{:table-of-contents} argument, if non-nil, indicates that output should include a table of contents if the style supports it."))))))
  (:properties (manual-section defdoc-via-asdf)))
(ensure-api-documentation :asdf-defdoc)

(def-output-class (defdoc-manual
                      :title "DefDoc user manual" :author "John Maraist"
                      :short-title "DefDoc"
                      :leader (:latex "DefDoc allows programmers and authors to base all program documentation and manuals on declarations within the program source.  By generating the manual, reference card, and docstrings from a common set of declarations, DefDoc helps eliminate the possibility of documents containing inconsistent, outdated material."))
  (collect-output (:title "Basic operations" :short-title "Basics")
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '((docspecs :order (def-documentation
                                                       def-target-type)
                                            :title "Writing documentation"
                                            :short-title "Writing docs"
                                   :leader (:latex "DefDoc uses a small set of macros for adding documentation to source code.  Its primary macro is \\texttt{def-documentation}, which attaches documentation to a target.  Targets include, by default, anything to which the Lisp \\texttt{documentation} model applies --- \\texttt{function}s, \\texttt{type}s, \\texttt{compiler-macro}s and so forth, as well as individual generic function methods; authors can also create additional target types."))
                                  label-use
                                  (outspec
                                   :order (def-output-class def-label-config
                                              collect-target-type)
                                   ;; :title "Packaging units of output"
                                   :leader (:latex "DefDoc separates the description of the \\emph{contents} of an output document from the mechanics of generating the document.  The description of a document's contents is though \\emph{output units}.  DefDoc provides macros and functions to quickly specify output units.  The top-level macro for these definitions is \\emph{def-output-class}, which creates a class corresponding to an output unit, and an initialiation method for gathering its contents.  The initialiation methods are based on calls to various \\texttt{collect} functions and macros within the body of a \\emph{def-output-class} call."))
                                  (defdoc-via-asdf
                                   :leader (:latex "DefDoc integrates with ASDF to coordinate documentation generation.  Currently, DefDoc provides an interim base ASDF system, under which the ASDF \\texttt{load-op} triggers document generation.  We plan to develop a second base system which supports a new operator \\texttt{doc-op}, with distinguished components loaded only for the documentation, and not the standard component load or test."))
                                  (styles :title "The output style"
                                   :short-title "Output style"
                                   :order (write-output plaintext-style
                                           latex-style itemized-list-style symbol-homing-style
                                           html-style)
                                   :leader (:latex "This section discusses \\emph{styles}, classes encapsulating various options for documentation generation.  Style classes allow output document generation to be consolidated into a single generic function \\texttt{write-output}.  DefDoc exports a number of pre-defined output styles."))
                                  (latex :title (:latex "Other \\LaTeX\\ generators")
                                   :short-title (:latex "\\LaTeX")
                                   :order (write-spec-latex
                                           write-doctype-latex
                                           write-package-specs-latex)
                                   :leader (:latex "DefDoc's facilities for generating documentation fully from output unit definitions is attractive, but is not always feasible, in particular for existing projects to which DefDoc is gradually applied.  This section documents the API for an alternative mode for using DefDoc for \\LaTeX\\ documentation, where snippets of generated \\LaTeX\\ source are included in a manually-constructed top-level \\LaTeX\\ document.  These functions, for the most part, wrap a call to the \\texttt{format-doc} or \\texttt{format-docspec} control API functions with stream-opening and other administrative environment setup."))
                                  (html :title "Other HTML generators"
                                   :short-title "HTML")
                                  (newtargetdef
                                   :title "Defining target types"
                                   :short-title "Defining targets"
                                   :leader (:seq "DefDoc includes target type definitions for all of the legal targets of Common Lisp documentation: " (:lisp symbol function) ", " (:lisp symbol compiler-macro) ", " (:lisp symbol setf) ", " (:lisp symbol method-combination) ", " (:lisp symbol type) ", " (:lisp symbol structure) " and " (:lisp symbol variable) " as well as " (:lisp symbol method) ", " (:lisp symbol symbol) " and " (:lisp symbol keyword) ".  Defining additional target types is straightforward."))))
      (collect-exported-symbols :defdoc)
      (collect-symbols #:asdf #:defdoc-asdf)))
  (collect-output (:title "Customizing and extending styles"
                   :short-title "Styles"
                   :leader (:seq "This section describes how the standard document style classes work, and how they can be customized and extended.  The "
                                 (:emph "content")
                                 " of output documents is specified separately from their "
                                 (:emph "layout")
                                 ": contents have a uniform internal representation, which is converted, according to method dispatching on style classes, to plain text, "
                                 (:latex-name)
                                 ", HTML, or other document forms.  We first describe the "
                                 (:emph "standard documentation model")
                                 " into which "
                                 (:lisp compiler-macro def-documentation)
                                 " translates documentation, the "
                                 (:emph "output model")
                                 " into which "
                                 (:lisp compiler-macro def-output-class)
                                 " translates output contents, and the general functions which traverse output contents.  We then present the extensions to the output model for "
                                 (:latex-name)
                                 " and HTML generation.  Finally, there are macros which check style implementations to ensure adequate method definitions."))
    (collect-output
        (:title "Documentation model"
                :short-title "Docs model"
                :leader (:seq "DefDoc provides a standard documentation model into which "
                              (:lisp compiler-macro def-documentation)
                              " translates documentation by default, although package authors can override this representation. In this section we distinguish the "
                              (:emph "core")
                              " model, which all models must extend, from the particulars of the standard model."))
      (collect-groups-by-label
          (manual-section :package :defdoc
                          :groups '((model :title "The core document model"
                                     :short-title "Core model"
                                     :order (doc-spec docspec-element)
                                     :leader (:seq "The internal model of the documentation attached to Lisp artifacts is based around two top-level superclasses.  The " (:lisp type doc-spec) " class collects all of the different documenting elements associated with an artifact. In the standard model, elements can include a short blurb, a text introduction, an explanation of function arguments, specifications of function calls, and a detailed description.  A " (:lisp type doc-spec) " instance contains " (:lisp type docspec-element) " instances modeling each element. Subclasses of " (:lisp type docspec-element) " provided in the standard model include atomic elements such as a snippet of plain text or " (:latex-name) ", as well as aggregating elements such as sequences of paragraphs or itemized lists."))
                                    (standard-model
                                     :title "Standard documentation specifications"
                                     :short-title "Standard specs"
                                     :order (standard-doc-spec))
                                    (standard-model-elements
                                     :title "Specification elements"
                                     :short-title "Elements"
                                     :leader (:seq "A documentation spec as implemented by "
                                              (:lisp type doc-spec)
                                              " and its subclasses encapsulates all of the information to be associated a Lisp entity.  Each documentation spec may contain multiple, distinct textual "
                                              (:emph "elements")
                                              ".  The different element types include atomic plain or styled text, or references to Lisp elements, as well as more complicated aggregators of subelements into paragraph lists, or itemized or enumerated lists. This section details the standard extensions to " (:lisp type docspec-element) " provided by DefDoc.")
                                     :order (standard-doc-element
                                             def-element
                                             standard-plain-text
                                             standard-lisp-name
                                             standard-code
                                             standard-inline
                                             standard-fillin-place
                                             standard-emphasized
                                             standard-sequence
                                             standard-paragraph-list
                                             standard-simple-list-environment
                                             standard-enumerate
                                             standard-itemize
                                             standard-outputset-element))
                                    (standard-model-callspecs
                                     :title "Call specifications"
                                     :short-title "Call specs"
                                     :order (standard-callspec))
                                    (output-model
                                     :title "Output document structure"
                                     :short-title "Output structure"
                                     :order (output-contents
                                             explicit-doc-element))))
        (collect-exported-symbols :defdoc-control-api)))
    (collect-output (:title "Formatting standard output documents"
                            :short-title "Formatting output")
      (collect-groups-by-label
          (manual-section
           :package :defdoc
           :groups '((standard-model-spec-formatting
                      :title "Documentation specifications"
                      :short-title "Specs")
                     (standard-model-element-formatting
                      :title "Documentation elements"
                      :short-title "Elements")
                     (standard-model-output-formatting
                      :title "Output documents"
                      :short-title "Output")))
        (collect-exported-symbols :defdoc-control-api)))
    (collect-output (:title "Plaintext styles" :short-title "Plaintext")
      (collect-groups-by-label
          (manual-section
           :package :defdoc
           :groups '((plaintext-docstrings
                      :title "Retrieving traditional docstrings"
                      :short-title "Docstrings")
                     (plaintext-utils
                      :title "Some useful functions"
                      :short-title "Functions"
                      :order (output-lines))))
        (collect-exported-symbols :defdoc-control-api)))
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '(plaintext
                                  (latex-style-model
                                   :order (standard-latex
                                           full-package-latex-style-mixin
                                           package-list-latex-mixin))
                                  html-style-model
                                  (api-checks :title "Method verification"
                                   :short-title "Methods")))
      (collect-exported-symbols :defdoc-control-api)))
  (collect-output (:title "Customizing documentation models"
                          :short-title "Customization")
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '(doc-gen control targets label-model
                                  elements))
      (collect-exported-symbols :defdoc)
      (collect-exported-symbols :defdoc-control-api)))
  (collect-groups-by-label
      (manual-section :package :defdoc :groups '(deprecated))
    (collect-exported-symbols :defdoc)
    (collect-exported-symbols :defdoc-control-api)))

(defclass manual-style (symbol-homing-style
                        latex-style
                        docspec-fancy-header-latex-style) ())
(defclass sublist-style (symbol-homing-style latex-style itemized-list-style)
  ())
(defmethod candidate-home-packages ((style manual-style) target-type spec)
  (declare (ignore target-type spec))
  '(:defdoc-control-api :asdf-defdoc :defdoc))
(defmethod get-included-outputset-style ((outer manual-style)
                                         inner-style contained)
  (declare (ignore inner-style contained))
  'sublist-style)

(def-label-config (:label manual-section :package :defdoc-docsyms)
  (docspecs :title "Providing documentation")
  (label-use :title "Annotating documentation with labels")
  (outspec :title "Packaging units of output")
  (defdoc-via-asdf :title "Document generation via ASDF")
  (control :title "Control model")
  (styles :title "Mixins for styles")
  (targets :title "Documentation targets")
  (model :title "Documentation models")
  (elements :title "Documentation model elements")
  (label-model :title "Accessing labels and values")
  (output-model :title "Output models")
  (plaintext :title "Generating plain text")
  (latex :title (:latex "Generating \\LaTeX"))
  (latex-style-model :title (:latex "The \\LaTeX\\ model"))
  (html-style-model :title (:latex "The HTML model"))
  (deprecated :title "Deprecated forms"))
