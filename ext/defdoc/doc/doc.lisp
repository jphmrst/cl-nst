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
  (:details (:seq
             "For example"
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
  (:properties (manual-section defdoc::asdf-defdoc)))

(def-output-class (defdoc-manual
                      :title "DefDoc user manual" :author "John Maraist")

  (collect-doc ()
               "DefDoc allows programmers and authors to base all program documentation and manuals on declarations within the program source.  By generating the manual, reference card, and docstrings from a common set of declarations, DefDoc helps eliminate the possibility of documents containing inconsistent, outdated material.")
  (collect-output (:title "Basic operations")
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '((docspecs :order (def-documentation
                                                       def-target-type)
                                            :title "Writing documentation"
                                            :leader (:latex "DefDoc uses a small set of macros for adding documentation to source code.  Its primary macro is \\texttt{def-documentation}, which attaches documentation to a target.  Targets include, by default, anything to which the Lisp \\texttt{documentation} model applies --- \\texttt{function}s, \\texttt{type}s, \\texttt{compiler-macro}s and so forth, as well as individual generic function methods; authors can also create additional target types."))
                                  (outspec
                                   :order (def-output-class def-label-config
                                            collect-target-type)
                                   :leader (:latex "DefDoc separates the description of the \\emph{contents} of an output document from the mechanics of generating the document.  The description of a document's contents is though \\emph{output units}.  DefDoc provides macros and functions to quickly specify output units.  The top-level macro for these definitions is \\emph{def-output-class}, which creates a class corresponding to an output unit, and an initialiation method for gathering its contents.  The initialiation methods are based on calls to various \\texttt{collect} functions and macros within the body of a \\emph{def-output-class} call."))
                                  (asdf-defdoc
                                   :leader (:latex "DefDoc integrates with ASDF to coordinate documentation generation.  Currently, DefDoc provides an interim base ASDF system, under which the ASDF \\texttt{load-op} triggers document generation.  We plan to develop a second base system which supports a new operator \\texttt{doc-op}, with distinguished components loaded only for the documentation, and not the standard component load or test."))
                                  (styles :title "The output style"
                                   :order (write-output
                                           latex-style symbol-homing-style)
                                   :leader (:latex "This section discusses \\emph{styles}, classes encapsulating various options for documentation generation, which are exported by DefDoc."))
                                  (latex :title (:latex "Other \\LaTeX\\ generators"))))
      (collect-exported-symbols :defdoc)
      (collect-symbols #:asdf #:defdoc-asdf)))
  (collect-output (:title "Customizing and extending styles")
    (collect-output (:title "Documentation model")
      (collect-groups-by-label
          (manual-section :package :defdoc
                          :groups '((model :title "The core document model"
                                           :order (doc-spec))
                                    (standard-model
                                     :title "The standard document model"
                                     :order (standard-doc-spec
                                             standard-callspec
                                             standard-simple-list-environment
                                             standard-enumerate
                                             standard-itemize
                                             standard-code
                                             standard-output-framework
                                             standard-sequence))))
        (collect-exported-symbols :defdoc-control-api)))
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '((output-model :order (output-framework))
                                  plaintext
                                  (latex-style-model
                                   :order (standard-latex
                                           full-package-latex-style-mixin
                                           package-list-latex-mixin))))
      (collect-exported-symbols :defdoc-control-api)))
  (collect-output (:title "Customizing documentation models")
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
(defmethod candidate-home-packages ((style manual-style) target-type spec)
  (declare (ignore target-type spec))
  '(:defdoc-control-api :asdf-defdoc :defdoc))

(def-label-config (:style manual-style :label manual-section :package :defdoc)
  (docspecs :title "Providing documentation")
  (outspec :title "Packaging units of output")
  (asdf-defdoc :title "Document generation via ASDF")
  (control :title "Control model")
  (styles :title "Mixins for styles")
  (targets :title "Documentation targets")
  (model :title "Documentation models")
  (elements :title "Documentation model elements")
  (label-model :title "Accessing labels and values")
  (output-model :title "Output models")
  (standard-model :title "The standard documentation model")
  (plaintext :title "Generating plain text")
  (latex :title (:latex "Generating \\LaTeX"))
  (latex-style-model :title (:latex "The \\LaTeX\\ model"))
  (deprecated :title "Deprecated forms"))
