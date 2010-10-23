;;; File package.lisp
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
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :nst)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir))
         (manual-dir (merge-pathnames #p"manual/" doc-root-dir))
         (quickref-dir (merge-pathnames #p"quickref/" doc-root-dir)))
    (format t "Creating documentation in ~a~%" doc-root-dir)
    (defdoc:write-package-specs-latex :nst
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a for manual...~%" type name))
        :directory gen-dir
        :style 'nst-item-style
        :include-doctypes '(nst::criterion)
        :package-style 'nst-package-list-latex-style)
    (defdoc:write-package-specs-latex :nst
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a for quickref...~%" type name))
        :directory gen-dir
        :style 'nst-quickref
        :include-doctypes '(nst::criterion)
        :package-style nil)

    ;; Run LaTeX to build the manual
    (format t "Generating PDF manual from LaTeX...~%")
    #+allegro (progn
                 (excl:chdir manual-dir)
                 (excl:run-shell-command "pdflatex manual.tex")
                 (excl:run-shell-command "makeindex manual")
                 (excl:run-shell-command "pdflatex manual.tex")
                 (excl:run-shell-command "pdflatex manual.tex")
                 (excl:chdir quickref-dir)
                 (excl:run-shell-command "pdflatex quickref")
                 (excl:run-shell-command "pdflatex quickref"))

    #+sbcl (progn
             (sb-posix:chdir manual-dir)
             (sb-ext:run-program "pdflatex" '("manual.tex")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "makeindex" '("manual")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "pdflatex" '("manual.tex")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "pdflatex" '("manual.tex")
                                 :wait t :search t :output nil :error t)
             (sb-posix:chdir quickref-dir)
             (sb-ext:run-program "pdflatex" '("quickref.tex")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "pdflatex" '("quickref.tex")
                                 :wait t :search t :output nil :error t))

;;;    #+clozure (progn
;;;                (setf (current-directory) manual-dir)
;;;                (run-program "pdflatex" '("manual.tex")
;;;                             :wait t :search t :output nil :error t)
;;;                (run-program "makeindex" '("manual")
;;;                             :wait t :search t :output nil :error t)
;;;                (run-program "pdflatex" '("manual.tex")
;;;                             :wait t :search t :output nil :error t)
;;;                (run-program "pdflatex" '("manual.tex")
;;;                             :wait t :search t :output nil :error t)
;;;                (setf (current-directory) quickref-dir)
;;;                (run-program "pdflatex" '("quickref.tex")
;;;                             :wait t :search t :output nil :error t))

;;;    #+clisp (progn
;;;              (ext:cd manual-dir)
;;;              (ext:shell "pdflatex manual.tex")
;;;              (ext:shell "makeindex manual")
;;;              (ext:shell "pdflatex manual.tex")
;;;              (ext:shell "pdflatex manual.tex")
;;;              (ext:cd quickref-dir)
;;;              (ext:shell "pdflatex quickref.tex"))

;;;    #+lispworks (progn
;;;                  (hcl:change-directory manual-dir)
;;;                  (system:call-system "pdflatex manual.tex")
;;;                  (system:call-system "makeindex manual")
;;;                  (system:call-system "pdflatex manual.tex")
;;;                  (system:call-system "pdflatex manual.tex")
;;;                  (hcl:change-directory quickref-dir)
;;;                  (system:call-system "pdflatex quickref.tex"))

    #-(or allegro sbcl ;; clozure clisp lispworks
          )
    (warn "Documentation building not fully implemented on this system" manual-dir)))

;;; --------------------------------------------------
;;; Style for criteria --- prob. deprecated

(defclass nst-criterion-style (defdoc:latex-style
                               defdoc:package-list-latex-mixin) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-criterion-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))

;;; --------------------------------------------------
;;; Style for the manual.

(defclass nst-item-style (defdoc:latex-style) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-item-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc:latex-style-adjust-spec-element ((style nst-item-style)
                                                   target-type spec
                                                   (element (eql :intro))
                                                   datum)
  (declare (ignore datum target-type))
  (with-accessors ((self defdoc:docspec-self)) spec
    (make-instance 'defdoc:standard-sequence
      :elements (list (make-instance 'defdoc:standard-latex
                        :latex (format nil "\\label{~a:primary}" self))
                      (call-next-method)))))
(defmethod defdoc:latex-style-adjust-spec-element ((style nst-item-style)
                                                   target-type spec
                                                   (element (eql :short))
                                                   datum)
  (declare (ignore datum target-type))
  (with-accessors ((self defdoc:docspec-self)) spec
    (make-instance 'defdoc:standard-sequence
      :elements (list (make-instance 'defdoc:standard-latex
                        :latex (format nil "\\label{~a:primary}" self))
                      (call-next-method)))))

(defmethod format-docspec-element ((style nst-item-style)
                                   (target-type (eql 'nst::criterion))
                                   (spec defdoc:standard-doc-spec) stream)
  (with-accessors ((self defdoc:docspec-self)) spec
    (format stream "\\subsubsection{The \\texttt{~s} criterion}" self)
    (call-next-method)))

;;; --------------------------------------------------
;;; Style for the manual's package list.

(defclass nst-package-list-latex-style (defdoc:package-list-latex-mixin
                                        defdoc:latex-style) ())
(defmethod defdoc:package-list-entry ((style nst-package-list-latex-style)
                                      spec group entry stream)
     (declare (ignore spec group))
     (let ((self (defdoc:docspec-self entry)))
       (format stream
           "\\texttt{~a} --- \\S\\ref{~:*~a:primary}, p.\\,\\pageref{~:*~a:primary}.~%~%"
         self)))

;;; --------------------------------------------------
;;; Style for the quickref card.

(defclass nst-quickref (defdoc:latex-style) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-quickref)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod format-docspec-element ((style nst-quickref) target-type
                                   (spec defdoc:standard-doc-spec) stream)
  (defdoc:with-unpacked-standard-spec (self intro intro-supp-p params params-supp-p
                                     short short-supp-p full full-supp-p
                                     callspec) spec
    ;;(declare (ignore full full-supp-p))
    (cond
     (short-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style target-type spec
                                                       :intro short)
                      target-type))
     (intro-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style target-type spec
                                                       :intro intro)
                      target-type)))

    (when callspec
      (princ " \\begin{verbatim}" stream)
      (loop for (cs . others) on callspec do
        (loop for line
              in (defdoc:callspec-to-lines cs defdoc:*latex-verbatim-width* self)
              do
           (format stream "  ~a~%" line))
        (when others (format stream "~%")))
      (princ "\\end{verbatim}" stream))

    (when params-supp-p
      (princ "\\begin{description}" stream)
      (loop for (name subspec) in params do
        (format stream "\\item[~a] " name)
        (format-docspec stream style
                        (latex-style-adjust-spec-element style target-type spec
                              :subspec subspec)
                        target-type))
      (princ "\\end{description}" stream))))