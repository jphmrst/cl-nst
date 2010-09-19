(in-package :defdoc)

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

(def-documentation (compiler-macro def-doctype)
    (:callspec (name (&key) &body (:seq form))))

(def-documentation (compiler-macro def-spec-format)
    (:callspec (name formals &rest (:seq form))
               ((name) formals &rest (:seq form))))

(def-documentation (function get-doctypes)
    (:callspec ()))

(def-documentation (function format-docspec)
    (:callspec (stream style spec)))

(def-documentation (function format-docspec-element)
    (:callspec (spec-head stream style spec-args)))

(def-documentation (function write-spec-latex)
    (:callspec (name usage &key
                              (style style)
                              (directory pathname)
                              (file filename-string))))

(def-documentation (function write-package-specs-latex)
    (:callspec (package-specifier &optional echo directory style)))

(def-documentation (function spec-to-lines)
    (:callspec (spec width)))

(def-documentation (function indent-by)
    (:callspec (lines length)))

(def-documentation (function bracket-with)
    (:callspec (lines prefix suffix)))

(def-documentation (function width)
    (:callspec (lines)))

(def-documentation (function flow)
    (:callspec (formatter artifacts max)))

(def-documentation (type standard-docstring-style)
    (:short "The default style for docstring generation."))

(def-documentation (variable *docstring-style*)
    (:short "Style used when generating docstrings."))

(def-documentation (variable *defdoc-latex-default-directory*)
    (:short "Directory where generated latex files should be written."))
