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
(in-package :common-lisp-user)

(defpackage :defdoc
    (:documentation "Structured documentation definition")
    (:nicknames :ddoc)
    (:use :common-lisp)
    (:export #:def-documentation
             #:def-doctype
             #:def-spec-format
             #:def-doc-tag

             #:get-doctypes
             #:format-docspec #:format-docspec-element
             #:write-doctype-latex
             #:write-spec-latex
             #:write-package-specs-latex

             #:spec-to-lines
             #:indent-by #:bracket-with #:width #:flow

             #:latex-style-adjust-spec-element
             #:package-list-entry
             #:package-list-group-header
             #:package-list-overall-header
             #:get-latex-output-file-name

             #:standard-docstring-style
             #:latex-style
             #:package-list-latex-style
             #:*docstring-style*
             #:*defdoc-latex-default-directory*))

(defun defdoc::make-package-documentation ()
  "Write documentation for this package, using system package-doc."
  (asdf:oos 'asdf:load-op 'package-doc)
  (funcall (symbol-function (intern (symbol-name 'package-doc)
                                    (find-package :package-doc)))
           (find-package :defdoc)))

(defmacro defdoc::let-echo* (bindings &body body)
  (let ((x (gensym)))
    `(let* ,(loop for (name form) in bindings
                  collect `(,name (let ((,x ,form))
                                    (format t "~a = ~s~%" ',name ,x))))
       ,@body)))
