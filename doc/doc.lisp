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
    (defdoc:write-doctype-latex 'nst::criterion
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a...~%" type name))
        :directory gen-dir
        :style 'nst-criterion-style)
    (defdoc:write-package-specs-latex :nst
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a for manual...~%" type name))
        :directory gen-dir
        :style 'nst-item-style
        :package-style 'nst-package-list-latex-style)
    (defdoc:write-package-specs-latex :nst
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a for quickref...~%" type name))
        :directory gen-dir
        :style 'nst-quickref
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
    (warn "Documentation building not fully implemented on this system --- please run~%  pdflatex manual.tex~%  makeindex manual~%  pdflatex manual.tex~%from the directory~%  ~a~%" manual-dir)))

(defclass nst-criterion-style (defdoc:package-list-latex-style) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-criterion-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))

(defclass nst-item-style (defdoc:latex-style) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-item-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc:latex-style-adjust-spec-element ((style nst-item-style)
                                                   spec-type spec-head
                                                   (element (eql :intro))
                                                   spec-args datum)
  (declare (ignore datum spec-head spec-type))
  (destructuring-bind (&key self &allow-other-keys) spec-args
  `(:seq (:latex ,(format nil "\\label{~a:primary}" self))
         ,(call-next-method))))
(defmethod defdoc:latex-style-adjust-spec-element ((style nst-item-style)
                                                   spec-type spec-head
                                                   (element (eql :short))
                                                   spec-args datum)
  (declare (ignore datum spec-head spec-type))
  (destructuring-bind (&key self &allow-other-keys) spec-args
  `(:seq (:latex ,(format nil "\\label{~a:primary}" self))
         ,(call-next-method))))

(defclass nst-quickref (defdoc:latex-style) ())
(defmethod defdoc:get-latex-output-file-name ((style nst-quickref)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod format-docspec-element ((style nst-quickref) spec-type
                                   (in (eql :spec)) stream spec-args)
  (destructuring-bind (&key self
                            (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (callspec nil callspec-supp-p)
                       &allow-other-keys) spec-args
    (cond
     (short-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style spec-type in
                                                       :intro spec-args short)
                      spec-type))
     (intro-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style spec-type in
                                                       :intro spec-args intro)
                      spec-type)))

    (when callspec-supp-p
      (princ " \\begin{verbatim}" stream)
      (loop for (cs . others) on callspec do
        (loop for line
              in (defdoc::callspec-to-lines cs defdoc::*latex-verbatim-width* self)
              do
           (format stream "  ~a~%" line))
        (when others (format stream "~%")))
      (princ "\\end{verbatim}" stream))

    (when params-supp-p
      (princ "\\begin{description}" stream)
      (loop for (name subspec) in params do
        (format stream "\\item[~a] " name)
        (format-docspec stream style
                        (latex-style-adjust-spec-element style spec-type in
                              :subspec spec-args subspec)
                        spec-type))
      (princ "\\end{description}" stream))))

(defclass nst-package-list-latex-style (defdoc:package-list-latex-style) ())
(defmethod defdoc:package-list-entry ((style nst-package-list-latex-style)
                                      spec group entry stream)
     (declare (ignore spec group))
     (destructuring-bind (&key self &allow-other-keys) (cdr entry)
       (format stream
           "\\texttt{~a} --- \\S\\ref{~:*~a:primary}, p.\\,\\pageref{~:*~a:primary}.~%~%"
         self)))
