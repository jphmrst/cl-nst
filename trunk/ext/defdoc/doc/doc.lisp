;;; File doc.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
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
(in-package :defdoc-doc)

(defdoc:def-output-framework (defdoc-manual :title "DefDoc user manual"
                                            :author "John Maraist")

  (defdoc::collect-groups-by-label
      (defdoc::manual-section :package :defdoc
                              :groups '(defdoc::docspecs
                                        defdoc::outspec defdoc::doc-gen
                                        defdoc::control defdoc::targets
                                        defdoc::model defdoc::label-model
                                        defdoc::elements defdoc::standard-model
                                        defdoc::output-model defdoc::plaintext
                                        defdoc::latex defdoc::deprecated))
    (defdoc::collect-exported-symbols :defdoc)
    (defdoc::collect-exported-symbols :defdoc-control-api)))

(defclass manual-style (defdoc-control-api:latex-style) ())

(def-label-config (:style manual-style :label defdoc::manual-section
                          :package :defdoc)
    (docspecs :title "Providing documentation"
              :order (def-documentation))
  (outspec :title "Output documents"
           :order (def-output-framework def-label-config))
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

(defun build-defdoc-docs ()
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :defdoc)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir)))

    (defdoc:write-latex-output 'defdoc-manual
        :echo #'(lambda (&key &allow-other-keys)
                  (format t "Writing manual~%"))
        :directory gen-dir
        :standalone t
        :index t :table-of-contents t
        :style 'manual-style)
    (defdoc:process-latex-document gen-dir "defdoc-manual_manual-style"
                                   :index t)))

;;;(defdoc::old-def-output-framework old-defdoc-manual
;;;    ;; Set the style to be associated with this output set.
;;;    ;;
;;;    ;; (:style style-class)
;;;
;;;  (:exported-symbols :defdoc)
;;;  (:exported-symbols :defdoc-control-api)
;;;
;;;  (:grouping-label defdoc::manual-section)
;;;  (:groups defdoc::docspecs
;;;           defdoc::outspec defdoc::doc-gen defdoc::control
;;;           defdoc::targets defdoc::model defdoc::label-model defdoc::elements
;;;           defdoc::standard-model defdoc::output-model defdoc::plaintext
;;;           defdoc::latex defdoc::deprecated)
;;;
;;;  (:title "DefDoc user manual")
;;;  (:author "John Maraist"))
;;;
;;;(defun old-output-build ()
;;;  "Write documentation for this package, using system package-doc."
;;;  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :defdoc)
;;;                                                      "doc/"))
;;;         (gen-dir (merge-pathnames #p"gen/" doc-root-dir)))
;;;    (format t "Creating documentation in ~a~%" doc-root-dir)
;;;    (defdoc::old-write-latex-output 'old-defdoc-manual
;;;        :echo #'(lambda (&key &allow-other-keys)
;;;                  (format t "Writing manual~%"))
;;;        :directory gen-dir
;;;        :standalone t
;;;        :index t :table-of-contents t
;;;        :style 'manual-style)
;;;    (defdoc:process-latex-document gen-dir "old-defdoc-manual_manual-style"
;;;                                   :index t)))

