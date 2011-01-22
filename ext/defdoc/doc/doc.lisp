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

(def-documentation (type asdf-defdoc::defdoc-asdf)
  (:intro (:latex "Class \\texttt{defdoc-asdf} --- \\fbox{FILL IN}"))
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
                                  asdf-defdoc
                                  latex
                                  styles))
      (collect-exported-symbols :defdoc)
      (collect-documented-symbols :asdf-defdoc)))
  (collect-output (:title "Internals")
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '(doc-gen control targets model label-model
                                  elements output-model))
      (collect-exported-symbols :defdoc)
      (collect-exported-symbols :defdoc-control-api)))
  (collect-output (:title "Standard models")
    (collect-groups-by-label
        (manual-section :package :defdoc
                        :groups '(standard-model plaintext latex-style-model))
      (collect-exported-symbols :defdoc-control-api)))
  (collect-groups-by-label
      (manual-section :package :defdoc :groups '(deprecated))
    (collect-exported-symbols :defdoc)
    (collect-exported-symbols :defdoc-control-api)))

(defclass manual-style (symbol-homing-style
                        docspec-fancy-header-latex-style
                        latex-style) ())
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
  (latex-style-model :title (:latex "The \\LaTeX\ model"))
  (deprecated :title "Deprecated forms"))
