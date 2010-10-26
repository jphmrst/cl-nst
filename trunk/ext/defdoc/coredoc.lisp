(in-package :defdoc)

;;; From package.lisp

(defdoc:def-documentation (package :defdoc)
    (:short "Structured docstrings manager.")
  (:descriptive "Defdoc"))

;;; globals.lisp

(def-documentation (variable *docstring-style*)
    (:short "Style used when generating docstrings."))

(def-documentation (function format-docspec)
  (:short "Produce output from a documentation specification.")
  (:callspec (stream style spec target-type))
  (:params (stream "Output stream for the result.")
           (style "Object specifying the style for the output.")
           (spec "The documentation spec.")
           (type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods.")))

(def-documentation (function format-docspec-element)
  (:short "Produce output for one docspec element.")
  (:callspec (style target-type element stream))
  (:params (style "Object specifying the style for the output.")
           (target-type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods.")
           (element "The documentation spec.")
           (stream "Output stream for the result.")))

;;; storage.lisp

(def-documentation (function get-doc-spec)
  (:short "Retrieve a docspec for an item of a particular target type.")
  (:callspec (name target-type))
  (:params (name "Symbol naming the item.")
           (target-type "The target-type of the spec.")))

(def-documentation (setf get-doc-spec)
  (:short "Set the docspec for an item of a particular target type."))

;;; targetdef.lisp

(def-documentation (type standard-doc-target)
    (:short "The information we store about each target type.")
  (:params (name "Symbol naming the target type.")
           (docstring-installer "Function called to install a docstring.  Functions stored in this slots should take two argument: the symbolic name of the item being documented, and its docspec object.")))

(def-documentation (function get-doc-target-types)
    (:short "Returns a list of sybolic names of the target types currently in use.")
  (:callspec ((:opt symbol)))
  (:params (symbol "If provided, then the function returns all target types for which the given symbol has a documentation instance."))
  (:full "The result does not include target types which have been declared, but for which no documentation specifications have been declared."))

(def-documentation (function get-target-type)
    (:intro "Return the information record of a target type, which should be some subclass of \\texttt{standard-doc-target} or at least implement all of its reader methods.")
  (:callspec (name (:opt no-error)))
  (:params (name "Symbolic name of the target type.")
           (no-error "If nil (the default), the function will raise an error when a symbol which does not name a target type is looked up.")))

(def-documentation (compiler-macro def-target-type)
  (:intro "Macro for defining a new documentation target type.")
  (:callspec (name (&key (class class-name))
                   &body (:key-head docstring-installer (target-name target-spec) (:seq form))))
  (:params (name "Symbolic name of the new target type.")
           (class-name "Class to be used as a record for the stored target type's information.  The default is \\texttt{standard-doc-target}; if another class is used it must support the \\texttt{:name} initarg and \\texttt{docstring-installer} accessor.")
           (docstring-installer "Function which installs a standard Lisp document string for targets of this type.")))

;;; spec.lisp

(def-documentation (variable *spec-class*)
  (:short "The name of the class used by default as the representation for a documentation specification."))

(def-documentation (type doc-spec)
  (:short "Common superclass of documentation spec classes.  Accepts calls to the accessors \\texttt{docspec-self}, \\texttt{docspec-target-type} and \\texttt{docspec-tags}."))

(def-documentation (function docspec-self)
    (:short "Accessor on documentation objects to the symbolic name of the target of the documentation."))

(def-documentation (function docspec-target-type)
    (:short "Accessor on documentation objects to the symbolic name of the target type of the documentation."))

(def-documentation (function docspec-tags)
    (:short "Accessor on documentation objects to the tags associated with a particular piece of documentation."))

(def-documentation (type standard-doc-spec)
  (:short "The standard, default class used to represent documentation internally within defdoc.  Accepts calls to seven accessors \\texttt{docspec-descriptive}, \\texttt{docspec-intro}, \\texttt{docspec-short}, \\texttt{docspec-full}, \\texttt{docspec-params}, \\texttt{docspec-callspecs} and \\texttt{docspec-deprecated} as well as to the three accessors of the superclass doc-spec."))

(def-documentation (function docspec-descriptive)
    (:short "Accessor on standard documentation objects for one-word descriptive string for the target.  Used in FILL IN"))

(def-documentation (function docspec-intro)
    (:short "Accessor on standard documentation objects for the introduction element."))

(def-documentation (function docspec-short)
    (:short "Accessor on standard documentation objects for the short description element."))

(def-documentation (function docspec-full)
    (:short "Accessor on standard documentation objects for the documentation's main body."))

(def-documentation (function docspec-params)
    (:short "Accessor on standard documentation objects for documentation of the object's parameters."))

(def-documentation (function docspec-callspecs)
    (:short "Accessor on standard documentation objects for the callspec element."))

(def-documentation (function docspec-deprecated)
    (:short "Accessor on standard documentation objects for the deprecation flag."))

(def-documentation (compiler-macro with-unpacked-standard-spec)
    (:short "Macro providing deconstruction of \\texttt{standard-doc-spec} objects."))

;;; elementdef.lisp

                                        ; #:*default-element-class*
                                        ; #:standard-doc-element
                                        ; #:def-element

;;; elements.lisp

                                        ; #:standard-plain-text
                                        ; #:text-element-text
                                        ; #:standard-latex
                                        ; #:latex-element-latex
                                        ; #:standard-paragraph-list
                                        ; #:paragraphlist-element-items
                                        ; #:standard-sequence
                                        ; #:sequence-element-items
                                        ; #:standard-code
                                        ; #:code-element-string
                                        ; #:standard-simple-list-environment
                                        ; #:list-element-specs
                                        ; #:list-element-options
                                        ; #:list-element-env-tag
                                        ; #:standard-itemize
                                        ; #:standard-enumerate

;;; tag.lisp

                                        ; #:def-doc-tags
                                        ; #:def-doc-tag

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

(def-documentation (variable *latex-verbatim-width*)
    (:short "Maximum line length in verbatim mode"))

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

;;;(def-documentation (function write-package-specs-latex)
;;;    (:callspec (package-specifier &optional echo directory style)))


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
