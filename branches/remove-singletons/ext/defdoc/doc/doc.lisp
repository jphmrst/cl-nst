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

(defdoc:def-output-framework (defdoc-manual :title "DefDoc user manual"
                                            :author "John Maraist")

  (defdoc:collect-groups-by-label
      (defdoc::manual-section :package :defdoc
                              :groups '(defdoc::docspecs
                                        defdoc::outspec defdoc::doc-gen
                                        defdoc::control defdoc::targets
                                        defdoc::model defdoc::label-model
                                        defdoc::elements defdoc::standard-model
                                        defdoc::output-model defdoc::plaintext
                                        defdoc::latex defdoc::deprecated))
    (defdoc:collect-exported-symbols :defdoc)
    (defdoc:collect-exported-symbols :defdoc-control-api)))

(defclass manual-style (defdoc-control-api:latex-style) ())

(def-label-config (:style manual-style :label defdoc::manual-section
                          :package :defdoc)
    (docspecs :title "Providing documentation"
              :order (def-documentation))
  (outspec :title "Output documents"
           :order (def-output-framework def-label-config))
  (asdf-defdoc :title "Document generation via ASDF")
  (control :title "Control model")
  (target :title "Documentation targets")
  (model :title "Documentation models")
  (elements :title "Documentation model elements")
  (label-model :title "Accessing labels and values")
  (output-model :title "Output models")
  (standard-model :title "The standard documentation model")
  (plaintext :title "Generating plain text")
  (latex :title "Generating \\LaTeX")
  (deprecated :title "Deprecated forms"))