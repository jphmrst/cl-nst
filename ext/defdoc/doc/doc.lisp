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

(defdoc:def-output-framework defdoc-manual
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

  (:exported-symbols :defdoc)
  (:exported-symbols :defdoc-control-api)

  (:doc-title "DefDoc user manual")
  (:doc-author "John Maraist")

;;;  (:grouping-label nst-manual)
;;;  (:groups fixtures groups tests criteria)
  )

(defclass manual-style (defdoc-control-api:latex-style) ())

(defun build-defdoc-docs ()
  "Write documentation for this package, using system package-doc."
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :defdoc)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir)))
    (format t "Creating documentation in ~a~%" doc-root-dir)
    (defdoc:write-latex-output 'defdoc-manual
        :echo #'(lambda (&key &allow-other-keys)
                  (format t "Writing manual~%"))
        :directory gen-dir
        :standalone t
        :style 'manual-style)
    (defdoc:process-latex-document gen-dir "defdoc-manual_manual-style"
                                   :index t)))
