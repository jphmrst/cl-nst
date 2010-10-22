(in-package :defdoc)

;;; From package.lisp

(def-documentation (variable *docstring-style*)
    (:short "Style used when generating docstrings."))

;;; format.lisp

(def-documentation (function format-docspec-element)
    (:callspec (spec-head stream style spec-args)))

;;; storage.lisp


;;; targetdef.lisp

;;; spec.lisp

;;; elementdef.lisp

;;; elements.lisp

;;; tag.lisp

;;; macro.lisp

(def-documentation (compiler-macro def-documentation)
    (:callspec ((name) &body
                (:opt (:key-head :short string-or-docspec))
                (:opt (:key-head :intro string-or-docspec))
                (:opt (:key-head :params (:seq (name string-or-docspec))))
                (:opt (:key-head :callspec (:seq call-sequence)))
                (:opt (:key-head :full string-or-docspec)))
               (name &body
                     (:opt (:key-head :short string-or-docspec))
                     (:opt (:key-head :intro string-or-docspec))
                     (:opt (:key-head :params (:seq (name string-or-docspec))))
                     (:opt (:key-head :callspec (:seq call-sequence)))
                     (:opt (:key-head :full string-or-docspec)))))

;;; callspec.lisp
                                        ; #:standard-callspec
                                        ; #:callspec-sequence-of
                                        ; #:callspec-optional
                                        ; #:callspec-keyheaded
                                        ; #:callspec-keyarg

;;; block.lisp

(def-documentation (function indent-by)
    (:callspec (lines length)))

(def-documentation (function bracket-with)
    (:callspec (lines prefix suffix)))

(def-documentation (function width)
    (:callspec (lines)))

(def-documentation (function flow)
    (:callspec (formatter artifacts max)))

;;; plaintext.lisp

(def-documentation (type standard-docstring-style)
    (:short "The default style for docstring generation."))

                                        ; #:format-docspec
                                        ; #:callspec-to-lines
                                        ; #:callspec-item-to-lines
                                        ; #:output-lines

;;; latex.lisp

                                        ; #:*defdoc-latex-default-directory*
                                        ; #:*latex-full-package-item-header-macro*
                                        ; #:get-latex-output-file-name
                                        ; #:latex-style
                                        ; #:latex-style-adjust-spec-element
                                        ; #:full-package-latex-style-mixin
                                        ; #:package-list-overall-header
                                        ; #:package-list-group-header
                                        ; #:package-list-entry
                                        ; #:package-list-latex-mixin

(def-documentation (function write-spec-latex)
    (:callspec (name usage &key
                              (style style)
                              (directory pathname)
                              (file filename-string))))

                                        ; #:write-doctype-latex

(def-documentation (function write-package-specs-latex)
    (:callspec (package-specifier &optional echo directory style)))


;;; Disused.

;;;(def-documentation (compiler-macro def-doctype)
;;;    (:callspec (name (&key) &body (:seq form))))
;;;
;;;(def-documentation (compiler-macro def-spec-format)
;;;    (:callspec (name formals &rest (:seq form))
;;;               ((name) formals &rest (:seq form))))
;;;
;;;(def-documentation (function get-doctypes)
;;;    (:callspec ()))
;;;
;;;(def-documentation (function format-docspec)
;;;    (:callspec (stream style spec)))
;;;
;;;
;;;(def-documentation (function spec-to-lines)
;;;    (:callspec (spec width)))
;;;
;;;
;;;(def-documentation (variable *defdoc-latex-default-directory*)
;;;    (:short "Directory where generated latex files should be written."))
