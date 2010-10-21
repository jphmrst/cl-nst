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
    (:export #:*docstring-style* ; globals.lisp
             #:*latex-verbatim-width*

             ;; format.lisp
             #:format-docspec-element

             ;; storage.lisp
             #:get-doc-spec

             ;; targetdef.lisp
             #:standard-doc-target
             #:get-doc-target-types
             #:get-target-types
             #:def-target-type

             ;; spec.lisp
             #:*spec-class*
             #:doc-spec
             #:docspec-self
             #:docspec-target-type
             #:docspec-tags
             #:standard-doc-spec
             #:docspec-descriptive
             #:docspec-intro
             #:docspec-short
             #:docspec-full
             #:docspec-params
             #:docspec-callspecs
             #:docspec-deprecated
             #:with-unpacked-standard-spec


             #::accessor;; elementdef.lisp
             #:*default-element-class*
             #:standard-doc-element
             #:def-element

             ;; elements.lisp
             #:standard-plain-text
             #:text-element-text
             #:standard-latex
             #:latex-element-latex
             #:standard-paragraph-list
             #:paragraphlist-element-items
             #:standard-sequence
             #:sequence-element-items
             #:standard-code
             #:code-element-string
             #:standard-simple-list-environment
             #:list-element-specs
             #:list-element-options
             #:list-element-env-tag
             #:standard-itemize
             #:standard-enumerate

             ;; tag.lisp
             #:def-doc-tags
             #:def-doc-tag

             ;; macro.lisp
             #:def-documentation

             ;; callspec.lisp
             #:standard-callspec
             #:callspec-sequence-of
             #:callspec-optional
             #:callspec-keyheaded
             #:callspec-keyarg

             ;; block.lisp
             #:indent-by
             #:bracket-with
             #:width
             #:flow

             ;; plaintext.lisp
             #:standard-docstring-style
             #:format-docspec
             #:callspec-to-lines
             #:callspec-item-to-lines
             #:output-lines

             ;; latex.lisp
             #:*defdoc-latex-default-directory*
             #:*latex-full-package-item-header-macro*
             #:get-latex-output-file-name
             #:latex-style
             #:latex-style-adjust-spec-element
             #:full-package-latex-style-mixin
             #:package-list-overall-header
             #:package-list-group-header
             #:package-list-entry
             #:package-list-latex-mixin
                                        ; Top-level LaTeX invocation.
             #:write-spec-latex
             #:write-doctype-latex
             #:write-package-specs-latex))

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
