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

  (collect-doc () "this is a test")
  (collect-groups-by-label
      (manual-section :package :defdoc
                      :groups '((docspecs :order (def-documentation
                                                     def-target-type)
                                          :title "Documentation in code"
                                          :leader "Test 2.")
                                (outspec
                                 :order (def-output-class def-label-config))
                                asdf-defdoc
                                doc-gen control targets model
                                label-model elements standard-model output-model
                                plaintext latex deprecated))
    (collect-exported-symbols :defdoc)
    (collect-documented-symbols :asdf-defdoc)
    (collect-exported-symbols :defdoc-control-api)))

(defclass manual-style (defdoc-control-api:latex-style) ())

(def-label-config (:style manual-style :label manual-section :package :defdoc)
  (docspecs :title "Providing documentation")
  (outspec :title "Content collection")
  (asdf-defdoc :title "Document generation via ASDF")
  (control :title "Control model")
  (targets :title "Documentation targets")
  (model :title "Documentation models")
  (elements :title "Documentation model elements")
  (label-model :title "Accessing labels and values")
  (output-model :title "Output models")
  (standard-model :title "The standard documentation model")
  (plaintext :title "Generating plain text")
  (latex :title "Generating \\LaTeX")
  (deprecated :title "Deprecated forms"))
