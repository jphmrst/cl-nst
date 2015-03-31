;;; File quickref.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :nst-doc)

(defun build-quickref ()
  "Write documentation for this package, using system package-doc."
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :nst)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir))
         (quickref-dir (merge-pathnames #p"quickref/" doc-root-dir)))
    (format t "Creating quickref included documents in ~a~%" doc-root-dir)
    (defdoc:write-package-specs-latex :nst
        :echo (nst::named-function nst-docs-write-package-specs-latex
                (lambda (&key name type)
                  (format t "Writing ~a ~a for quickref...~%" type name)))
        :directory gen-dir
        :style 'nst-quickref
        :include-doctypes '(nst::criterion)
        :package-style nil)

    ;; Run LaTeX.
    ;; (format t "Generating new PDF manual from LaTeX...~%")
    ;; (defdoc:process-latex-document manual-dir "new-manual" :index t)
    (format t "Generating quickref from LaTeX...~%")
    (defdoc:process-latex-document quickref-dir "quickref")))

;;; --------------------------------------------------
;;; Style for the quickref card.

(defclass nst-quickref (defdoc:latex-style) ())
(defmethod defdoc-control-api:get-latex-output-file-name ((style nst-quickref)
                                                          usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc-control-api:format-docspec-element
    ((style nst-quickref) target-type
     (spec defdoc-control-api:standard-doc-spec) stream &key &allow-other-keys)
  (defdoc-control-api:with-unpacked-standard-spec
      (self intro intro-supp-p params params-supp-p
            short short-supp-p full full-supp-p
            callspec) spec
    ;; (declare (ignore full full-supp-p))
    (cond
     (short-supp-p
      (format-docspec stream style
                      (get-element-for-docspec-format style target-type spec
                                                       :intro short)
                      target-type))
     (intro-supp-p
      (format-docspec stream style
                      (get-element-for-docspec-format style target-type spec
                                                       :intro intro)
                      target-type)))

    (when callspec
      (princ " \\begin{verbatim}" stream)
      (loop for (cs . others) on callspec do
        (loop for line
          in (defdoc-control-api:callspec-to-lines style target-type cs
               defdoc:*latex-verbatim-width* self)
          do
          (format stream "  ~a~%" line))
        (when others (format stream "~%")))
      (princ "\\end{verbatim}" stream))

    (when params-supp-p
      (princ "\\begin{description}" stream)
      (loop for (name subspec) in params do
        (format stream "\\item[~a] " name)
        (format-docspec stream style
                        (get-element-for-docspec-format style target-type spec
                                                         :subspec subspec)
                        target-type))
      (princ "\\end{description}" stream))))
