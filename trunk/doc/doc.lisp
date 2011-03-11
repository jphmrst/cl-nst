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
(in-package :nst-doc)

(defun build-nst-docs ()
  "Write documentation for this package, using system package-doc."
  (build-quickref)
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :nst)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir))
         (manual-dir (merge-pathnames #p"manual/" doc-root-dir)))
    (format t "Creating documentation in ~a~%" doc-root-dir)
    (defdoc:write-package-specs-latex :nst
        :echo (nst::named-function nst-docs-write-package-specs-latex
                (lambda (&key name type)
                  (format t "Writing ~a ~a for manual...~%" type name)))
        :directory gen-dir
        :style 'manual-primary-style
        :include-doctypes '(nst::criterion nst::command nst::switch)
        :package-style 'nst-package-list-latex-style)
    (defdoc:write-latex-output 'package-api
        :echo (nst::named-function nst-docs-write-latex-output
                (lambda (&key &allow-other-keys)
                  (format t "Writing package-api document spec~%")))
        :directory gen-dir
        :standalone t
        :style 'manual-primary-style)

    ;; Run LaTeX.
    ;; (format t "Generating new PDF manual from LaTeX...~%")
    ;; (defdoc:process-latex-document manual-dir "new-manual" :index t)
    (format t "Generating PDF manual from LaTeX...~%")
    (defdoc:process-latex-document manual-dir "manual" :index t)))

;;; -----------------------------------------------------------------
;;; Style for criteria --- prob. deprecated

(defclass nst-criterion-style (defdoc:latex-style
                               defdoc-control-api:package-list-latex-mixin) ())
(defmethod defdoc-control-api:get-latex-output-file-name ((style nst-criterion-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))

;;; -----------------------------------------------------------------
;;; Formatting callspecs of NST interactive commands and properties.

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::switch))
                                         spec width (calling string))
  (declare (ignore style spec width))
  (cond
   ((< 0 (length calling))
    (concatenate 'string ":nst :set " calling " "))
   (t calling)))

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::command))
                                         spec width (calling string))
  (declare (ignore style spec width))
  (cond
   ((< 0 (length calling))
    (concatenate 'string ":nst " calling " "))
   (t calling)))

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::command))
                                         spec width (calling null))
  (declare (ignore style spec width))
  ":nst")

(defmethod defdoc-control-api:callspec-suffix (style (target-type (eql 'nst::command))
                                         spec width calling)
  (declare (ignore style spec width calling))
  "")

(defmethod defdoc-control-api:callspec-suffix (style (target-type (eql 'nst::switch))
                                         spec width calling)
  (declare (ignore style spec width calling))
  "")

;;; -----------------------------------------------------------------
;;; Output document specs.

(defdoc:def-output-class (package-api)
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

  (defdoc:collect-groups-by-label
      (nst::api-summary)
    (defdoc:collect-target-type 'function :package :nst)
    (defdoc:collect-target-type 'function :package :nst-control-api)))

(defclass nst-output-toplevel (output-contents)
  ((contribs :initform nil :initarg :contribs :reader contribs)))
