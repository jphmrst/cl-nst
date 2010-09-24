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
  (format t "*load-truename* ~s~%" *load-truename*)
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :nst)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir))
         (manual-dir (merge-pathnames #p"manual/" doc-root-dir)))
    (defdoc:write-doctype-latex 'nst::criterion
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a...~%" type name))
        :directory gen-dir
        :style 'defdoc:package-list-latex-style)
    (defdoc:write-package-specs-latex :nst
        :echo #'(lambda (&key name type)
                  (format t "Writing ~a ~a...~%" type name))
        :directory gen-dir
        :package-style 'defdoc:package-list-latex-style)

    ;; Run LaTeX to build the manual
    (format t "Generating PDF manual from LaTeX...~%")
    #+allegro (progn
                 (excl:chdir manual-dir)
                 ;; (asdf:run-shell-command "pdflatex" "manual.tex")
                 (excl:run-shell-command "pdflatex manual.tex")
                 (excl:run-shell-command "makeindex manual")
                 (excl:run-shell-command "pdflatex manual.tex"))

    #+sbcl (progn
             (sb-posix:chdir manual-dir)
             (sb-ext:run-program "pdflatex" '("manual.tex")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "makeindex" '("manual")
                                 :wait t :search t :output nil :error t)
             (sb-ext:run-program "pdflatex" '("manual.tex")
                                 :wait t :search t :output nil :error t))

;;;    #+clozure (progn
;;;                (setf (current-directory) manual-dir)
;;;                (run-program "pdflatex" '("manual.tex")
;;;                             :wait t :search t :output nil :error t)
;;;                (run-program "pdflatex" '("manual.tex")
;;;                             :wait t :search t :output nil :error t))

    #-(or allegro sbcl ;; clozure
          )
    (warn "Documentation building not fully implemented on this system --- please run~%  pdflatex manual.tex~%from the directory~%  ~a~%" manual-dir)))
