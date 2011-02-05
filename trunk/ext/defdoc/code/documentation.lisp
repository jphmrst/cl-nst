;;; File coredoc.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; DefDoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; DefDoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with DefDoc.  If not, see
;;; <http://www.gnu.org/licenses/>.

(defpackage :defdoc-coredoc
  (:documentation "DefDoc internal organizational package - self-documentation")
  (:use :defdoc-docsyms :common-lisp
        :defdoc-core :defdoc-collectors :defdoc-standard-model
        :defdoc-latex :defdoc-html :defdoc-plaintext)
  #+allegro (:import-from excl #:named-function))
(in-package :defdoc-coredoc)

;; This eval-when is necessary on in this use of this method.  When
;; "normal" applications use DefDoc, they can place this defmethod in
;; e.g. the package.lisp or some other source file which will be
;; loaded before other files are compiled.
(def-bare-string-element-tag :latex :package :defdoc-core)
(def-bare-string-element-tag :latex :package :defdoc-collectors)
(def-bare-string-element-tag :latex :package :defdoc-standard-model)
(def-bare-string-element-tag :latex :package :defdoc-plaintext)
(def-bare-string-element-tag :latex :package :defdoc-latex)
(def-bare-string-element-tag :latex :package :defdoc-html)
(def-bare-string-element-tag :latex :package :defdoc-coredoc)

(def-property-label manual-section ())

;;; -----------------------------------------------------------------
;;; Standard documentation elements

(def-target-type doc-element (:symbol-definition-checker element-type-p))

(def-documentation (doc-element :plain)
 (:intro "A \\texttt{:plain} element should have one other element in the list, a string of plain text, with no backend-specific formatting.")
 (:callspec (string)))

(def-documentation (doc-element :latex)
 (:intro "A \\texttt{:latex} element should have one other element in the list, a string of \\LaTeX\\ source.")
 (:callspec (string)))

(def-documentation (doc-element :paragraphs)
 (:intro "A \\texttt{:paragraphs} element represents a sequence of paragraphs corresponding to the rest of the list.")
 (:callspec ( (:seq string-or-docspec))))

(def-documentation (doc-element :seq)
 (:intro "A \\texttt{:seq} element will combine the rest of the element's list into a single paragraph.")
 (:callspec ( (:seq string-or-docspec))))

(def-documentation (doc-element :code)
 (:intro "A \\texttt{:code} element should have one other element in the list, a string of plain text, possibly including line breaks, which will be rendered exactly as it is as an example of code.")
 (:callspec (string)))

(def-documentation (doc-element :itemize)
 (:intro "The list items after an \\texttt{:itemize} element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of an itemized list.")
 (:callspec (() (:seq string-or-docspec))))

(def-documentation (doc-element :enumerate)
                (:intro "The list items after an \\texttt{:enumerate} element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of a numbered list.")
                (:callspec (() (:seq string-or-docspec))))

(def-output-class doc-elements
    (collect-target-type 'doc-element))

;;; -----------------------------------------------------------------
;;; Special callspec list headers

(def-target-type callspec-special (:symbol-definition-nocheck t))

(def-documentation (callspec-special :seq)
  (:intro "The \\texttt{:seq} symbol indicates that the remaining specifiers of the list should be repeated in sequence.")
  (:details "In the above example,"
            (:code "  (:seq call-sequence)")
            "would be rendered under the standard document style as:"
            (:code "  call-sequence ... call-sequence")
            "and"
            (:code "  (:seq (name (:seq val)))")
            "would be rendered as:"
            (:code "  (name val ... val) ... (name val ... val)")))

(def-documentation (callspec-special :bag)
  (:intro "The \\texttt{:bag} symbol specifies usage of the following specifiers once or more each in any order.")
  (:details "For example,"
            (:code "  (:bag simple (less simple)
        (rather (:seq less) simple))")
            "could be rendered as:"
            (:code " [ simple
    | (less simple)
    | (rather less ... less simple ]*")))

(def-documentation (callspec-special :alt)
  (:intro "The \\texttt{:alt} symbol indicates that exactly one of the list's specifiers should be satisfied, and the rest ignored.")
  (:details "For example,"
            (:code "  (:alt :light :medium :dark)")
            "would be rendered as:"
            (:code " [ :light | :medium | :dark ]")))

(def-documentation (callspec-special :opt)
  (:intro "The \\texttt{:opt} symbol specifies optional forms.")
  (:details (:code "  (:opt (name (:seq val)))")
            "would be rendered as:"
            (:code "  [ (name val ... val) ]")))

(def-documentation (callspec-special :key-head)
  (:intro "The \\texttt{:key-head} symbol specifies a list headed by a keyword argument, and the form of the other values in that list.")
  (:details "In the example above,"
            (:code "  (:key-head blurb string-or-docspec)")
            "would be rendered as:"
            (:code "  (:blurb string-or-docspec)")
            "The \\texttt{:key-head} specifier removes any ambiguity in the callspecs of macro forms such as the body forms of a \\texttt{defclass}."
            (:code "  (:key-head seq item)")
            "would render as:"
            (:code "  (:seq item)")))

(def-output-class callspec-specials
    (collect-target-type 'callspec-special))

;;; -----------------------------------------------------------------
;;; Main macros


(def-documentation (compiler-macro def-documentation)
  (:intro "The \\texttt{def-documentation} macro attaches user documentation to a function, macro, or other documentable entity.")
  (:callspec (name &body
                   (:opt (:key-head blurb string-or-docspec))
                   (:opt (:key-head :intro string-or-docspec))
                   (:opt (:key-head :params (:seq (name string-or-docspec))))
                   (:opt (:key-head :callspec (:seq call-sequence)))
                   (:opt (:key-head :details string-or-docspec))
                   (:opt (:key-head :properties (:seq (prop-name value))))))
  (:params (string-or-docspec
            (:paragraphs
             "Actual documentation can be given as either a \\emph{docspec element}, a list with a recognized keyword as its first value, or as a plain string.  The standard docspec elements are:"
             (:itemize ()
               (:seq
                "A \\texttt{:plain} element should have one other element in the list, a string of plain text, with no backend-specific formatting."
                (:code "(:plain string)"))
               (:seq
                "A \\texttt{:latex} element should have one other element in the list, a string of \\LaTeX\\ source."
                (:code "(:latex string)"))
               (:seq
                "A \\texttt{:paragraphs} element represents a sequence of paragraphs corresponding to the rest of the list."
                (:code "(:paragraphs string-or-docspec
                           ... string-or-docspec)"))
               (:seq
                "A \\texttt{:seq} element will combine the rest of the element's list into a single paragraph."
                (:code "(:seq string-or-docspec ... string-or-docspec)"))
               (:seq
                "A \\texttt{:code} element should have one other element in the list, a string of plain text, possibly including line breaks, which will be rendered exactly as it is as an example of code."
                (:code "(:code string)"))
               (:seq
                "The list items after an \\texttt{:itemize} element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of an itemized list."
                (:code "(:itemize () string-or-docspec
                           ... string-or-docspec)"))
               (:seq
                "The list items after an \\texttt{:enumerate} element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of a numbered list."
                (:code "(:enumerate () string-or-docspec
                           ... string-or-docspec)")))
             "Additional docspec keywords can be declared with the \\texttt{def-element} macro.  The interpretation of a plain string is set by methods of the \\texttt{string-implicit-symbol-head} function."))
           (blurb "Short version of the item's documentation.  The blurb is exected to be a sentence fragment starting with a verb (``Provides support...'') rather than a complete sentence (``The \\texttt{give-support} function provides support...'').")
           (intro "Introductory section of the item's full documentation.  Generally, will be displayed before the usage specifications, parameter list and explanation body.")
           (params "Documentation of individual parameters.  Generally, the names used here should echo the names in the usage specifications.")
           (callspec (:paragraphs
                      "Specification of how a function of macro should be used.  These usage specifications are intended to be more verbose and thus more useful than verbatim lambda lists.  To that end, usage specifications may contain a number of special constructs in the form of a list with a keyword element as its first element."
                      (:seq
                       "For example, the callspec of the \\texttt{def-documentation} macro rendered above is:"
                       (:code "(name &body
  (:opt (:key-head :blurb string-or-docspec))
  (:opt (:key-head :intro string-or-docspec))
  (:opt (:key-head :params
                   (:seq (name string-or-docspec))))
  (:opt (:key-head :callspec (:seq call-sequence)))
  (:opt (:key-head :details string-or-docspec))
  (:opt (:key-head :properties
                   (:seq (prop-name value)))))"))
                      "Symbols which, as the first element of a list, indicate a special construct are:"
                      (:itemize ()
                        ("The \\texttt{:seq} symbol indicates that the remaining specifiers of the list should be repeated in sequence.  In the above example,"
                         (:code "  (:seq call-sequence)")
                         "would be rendered under the standard document style as:"
                         (:code "  call-sequence ... call-sequence")
                         "and"
                         (:code "  (:seq (name (:seq val)))")
                         "would be rendered as:"
                         (:code "  (name val ... val) ... (name val ... val)"))
                        (:seq
                         "The \\texttt{:bag} symbol specifies usage of the following specifiers once or more each in any order.  For example,"
                         (:code "  (:bag simple (less simple)
        (rather (:seq less) simple))")
                         "could be rendered as:"
                         (:code " [ simple
    | (less simple)
    | (rather less ... less simple ]*"))
                        (:seq
                         "The \\texttt{:alt} symbol indicates that exactly one of the list's specifiers should be satisfied, and the rest ignored.  For example,"
                         (:code "  (:alt :light :medium :dark)")
                         "would be rendered as:"
                         (:code " [ :light | :medium | :dark ]"))
                        ("The \\texttt{:opt} symbol specifies optional forms:"
                         (:code "  (:opt (name (:seq val)))")
                         "would be rendered as:"
                         (:code "  [ (name val ... val) ]"))
                        ("The \\texttt{:key-head} symbol specifies a list headed by a keyword argument, and the form of the other values in that list.  In the example above,"
                         (:code "  (:key-head blurb string-or-docspec)")
                         "would be rendered as:"
                         (:code "  (:blurb string-or-docspec)")
                         "The \\texttt{:key-head} specifier removes any ambiguity in the callspecs of macro forms such as the body forms of a \\texttt{defclass}."
                         (:code "  (:key-head seq item)")
                         "would render as:"
                         (:code "  (:seq item)")))
                      "Otherwise, lists in the callspec are taken to be a structured list argument, as for a macro lambda-list."))
           (full "Discussion section of the item's full documentation.  Generally, will be displayed after the introduction, usage specifications and parameter list.")
           (properties "List of property-value pairs to be associated with this documentation.  The property names should have be defined with the \\texttt{def-property-label} macro."))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro def-bare-string-element-tag)
  (:intro "The \\texttt{def-bare-string-element-tag} macro specifies how an unannotated string in a documentation spec should be interpreted.")
  (:callspec (tag &key (package name-or-package) (spec-type class-name)))
  (:params (tag "Keyword tag within which the bare string should be wrapped.")
           (name-or-package "Names the package within which this default applies.")
           (class-name "Allows a particular documentation specification base type to qualify the default value."))
  (:details (:latex "By default, a string will be interpreted as plain text.  In other words, the generic function for which this macro encapsulates a method declaration returns, by default \\texttt{:plain}, so that a bare \\texttt{string} is wrapped as:")
            (:code "  (:plain string)"))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro ensure-api-documentation)
  (:intro "DefDoc provides \\texttt{ensure-api-documentation} as a top-level form to check that all symbols in a package are provided documentation.")
  (:callspec (package &key (react flag) (error flag) (warning flag)
                           (internal flag) (defdoc-only flag)))
  (:params (package (:latex "The package to be checked"))
           (error (:latex
                   "If non-nil, raise an error on an undocumented symbol."))
           (warning (:latex "If the \\texttt{:error} argument is non-nil, then this argument is ignored.  Otherwise, if non-nil a \\texttt{warning} (as opposed to a \\texttt{style-warning} if \\texttt{null}), is raised on an undocumented symbol."))
           (internal (:latex "If non-nil, all symbols of the package are examined, not just the exported symbols."))
           (defdoc-only (:latex "If non-nil, only documentation provided via DefDoc is considered ``passing'' for this check.  Otherwise, the check will accept regular Lisp docstrings.")))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro def-property-label)
  (:intro "The \\texttt{def-property-label} macro declares a label for use in the \\texttt{:properties} of a documentation spec.")
  (:callspec (label-name () &body (:opt (:key-head :default-subsort name))))
  (:properties (manual-section label-use)))

(def-documentation (compiler-macro def-label-config)
  (:intro "The \\texttt{def-label-config} macro specifies information associated with particular uses, especially particular outputs, of a property label.")
  (:callspec ((&key (label label-name)
                    (style style)
                    (output-framework output-name)
                    (package package))
              &body (:seq (value &key (title title-spec-element)
                                 (order symbol-list)))))
  (:params (label-name "The name of a label to be configured.  Note that you do \\emph{not} need to specify label: this macro translates to internal method declarations; omitting a label could be used, for example, to give names to certain values of all label in the context of a particular style.")
           (style "If provided, names the style class to which this configuration should apply.")
           (output-name "If provided, the output unit name to which this configuration should apply.")
           (package "The package in which the value and label names given in this form should be interned.  By default, they are left in the current package.  Forward declarations cannot be used here; the package will be referenced when the \\texttt{def-label-config} macro is expanded.")
           (value "Designates the property value with which certain values should be associated.")
           (title-spec-element "Spec element corresponding to the section title associated with a particular label value.")
           (symbol-list "Order of display associated with a particular label value."))
  (:properties (manual-section label-use)))


;;; -----------------------------------------------------------------
;;; outspec

(def-documentation (compiler-macro def-output-class)
  (:intro "The \\texttt{def-output-class} macro defines the contents of an output document.")
  (:callspec ((output-class-name &key (class base-class)
                                 (title title-spec)
                                 (author author-spec))
              &body
              (:seq collection-form)))
  ;; (:details )
  (:params (output-class-name
            "The name of the class being created for this output specification")
           (base-class "Base class for the definition, by default \\texttt{output-contents}.")
           (title-spec "The title of the document component.")
           (author-spec "The author of the document component.")
           (collection-form "One or more calls to the \\texttt{collect-} functions below."))
  (:properties (manual-section outspec)))

(def-documentation (type output-contents)
  (:intro "The \\texttt{output-contents} class --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (type grouped-output-contents)
  (:intro "The \\texttt{grouped-output-contents} class --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-doc)
  (:intro "Function \\texttt{format-doc} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function get-grouped-output-group)
  (:intro "The \\texttt{get-grouped-output-group} function --- \\fbox{FILL IN}")
  (:callspec (grouped-output))
  (:properties (manual-section output-model)))

(def-documentation (function get-grouped-output-labeldef)
  (:intro "The \\texttt{get-grouped-output-labeldef} function --- \\fbox{FILL IN}")
  (:callspec (grouped-output))
  (:properties (manual-section output-model)))

(def-documentation (compiler-macro collect-groups-by-label)
  (:intro "Macro \\texttt{collect-groups-by-label} forms groups of output units by grouping according to property label values.")
  (:callspec ((label-name
               &key (package package)
               (groups ((:bag label-value
                              (label-value &key
                                           (order contents-order)
                                           (title section-title)
                                           (leader leading-doc)
                                           (trailer trailing-doc)))))
                         (exhaustive boolean))
              &body (:seq collection-form)))
  (:params (label-name "Symbol naming the label according to whose values groups will be formed.")
           (package "The Lisp package where, by default, the symbols in the macro call are taken to be.")
           (label-value "A label value which should be included in the generated output units.")
           (contents-order "A list of symbols specifying the basic order of selected elements.  By default, elements not included in the list follow those that are.")
           (section-title "Document element for the title of the sectioning unit formed from this group.")
           (leading-doc "Document element to be included before the specifications of this group")
           (trailing-doc "Document element to be included after the specifications of this group.")
           (boolean "If non-\\texttt{nil}, indicates that none of the document specifications accumulated by the \\texttt{collection-form}s should be omitted from the groups generated by this form.  For example, if some specs' label values are not included in the \\texttt{:groups} list, and this flag is set, then the macro will issue a warning.")
           (collection-form "One or more calls to \\texttt{collect-} functions which will be used to select the documentation specs to be grouped."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-doc)
  (:intro "Macro \\texttt{collect-doc} accumulates a single output element from a static document specification.  This macro allows literal text to be inserted into an output unit.")
  (:callspec (options (:seq form)))
  (:params (form (:latex "The forms are assembled into a document element."))
           (options (:latex "In this version, no options are recognized and this argument is ignored.")))
  (:details (:seq "For example:")
            (:code "(collect-doc ()
  (:latex \"And speaking of which:\")
  (:enumerate ()
    (:latex \"This stuff is cool.\")
    (:latex \"This next stuff is confusing.\")))"))
  (:properties (manual-section outspec)))

(def-documentation (function collect-target-type)
  (:intro "Function \\texttt{collect-target-type} aggregates documentation specifications for the \\texttt{def-output-class} macro according to the target type to which the specifications is attached --- all functions, all compiler macros, or more typically, all of a particular user-defined target type.")
  (:params (target-name "The target type of interest.")
           (filter (:paragraphs
                    "The filters in \\texttt{collect} functions specify further conditions for inclusion in the \\texttt{collect}ed result.  The filter itself is a list of keyword parameters.  Currently the \\texttt{collect} functions support one filter:"
                    (:itemize ()
                      (:latex "The \\texttt{:package} filter collects only specifications documenting symbols in the given package.")))))
  (:callspec (target-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-exported-symbols)
  (:intro "Function \\texttt{collect-exported-symbols} collects documentation from the symbols exported from a particular package.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for \\texttt{collect-target-type}."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-symbols)
  (:intro "The \\texttt{collect-symbols} macro simply accumulates the documentation of the named symbols.")
  (:callspec (package symbol-list (:seq filter)))
  (:params (package "The package in which the symbols to be collected are interned.  Note that this package need not necessarily exist for the \\texttt{def-output-class} to be compiled; DefDoc will not attempt to access the package until the output unit is instantiated.")
           (symbol-list "Symbols to be collected.")
           (filter "As for \\texttt{collect-target-type}."))
  (:properties (manual-section outspec)))

(def-documentation (function collect-documented-symbols)
  (:intro "Function \\texttt{collect-documented-symbols} collects documentation from all symbols in the package, exported or not, which have documentation explicitly declared for them.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for \\texttt{collect-target-type}."))
  (:properties (manual-section outspec)))

(def-documentation (function collect-all-symbols)
  (:intro "Function \\texttt{collect-all-symbols} collects documentation for all symbols in a package.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for \\texttt{collect-target-type}."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-output)
  (:intro "Function \\texttt{collect-output} allows included output class declarations to be nested within the including unit.")
  (:callspec (((:opt output-class-name)
               &key (class base-class) (title title-spec) (author author-spec))
              &body
              (:seq collection-form))
             (output-class-name &body (:seq collection-form)))
  (:details "Arguments are just as for \\texttt{def-output-class}; this call essentially just expands to a call to \\texttt{collect-named-output} with the new \\texttt{def-output-class} prepended to the present one.  If no \\texttt{output-class-name} is present, the result of a call to \\texttt{(gensym)} is used.")
  (:properties (manual-section outspec)))

(def-documentation (function collect-named-output)
  (:intro "Function \\texttt{collect-named-output} includes the instantiation of a named output unit class as a component of another output unit.")
  (:callspec (output-unit-name))
  (:params (output-unit-name "The output unit class to be included."))
  (:details "DefDoc does \\emph{not} check for (but is vulnerable to) circular inclusion of output units.")
  (:properties (manual-section outspec)))

;;; -----------------------------------------------------------------
;;; control

(def-documentation (function string-implicit-symbol-head)
  (:intro "The generic function \\texttt{string-implicit-symbol-head} directs how a bare string is converted into a documentation spec element.")
  (:callspec (package doc-spec string))
  (:properties (manual-section control)))

(def-documentation (function get-doc-spec)
  (:intro "The \\texttt{get-doc-spec} function retrieves a docspec for an item of a particular target type.")
  (:callspec (name target-type))
  (:params (name "Symbol naming the item.")
           (target-type "The target-type of the spec."))
  (:properties (manual-section control)))

(def-documentation (function get-spec-class)
  (:intro "Function \\texttt{get-spec-class} --- \\fbox{FILL IN}")
  (:callspec (package name form-list))
  (:properties (manual-section control)))

(def-documentation (function compile-spec)
  (:intro "Function \\texttt{compile-spec} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))

(def-documentation (setf get-doc-spec)
  (:blurb "Used with \\texttt{setf}, \\texttt{get-doc-spec} sets the docspec for an item of a particular target type.")
  (:properties (manual-section control)))


;;; -----------------------------------------------------------------
;;; targets

(def-documentation (function get-doc-target-types)
    (:intro "The \\texttt{get-doc-target-types} function returns a list of sybolic names of the target types currently in use.")
  (:callspec ((:opt symbol)))
  (:params (symbol "If provided, then the function returns all target types for which the given symbol has a documentation instance."))
  (:details "The result does not include target types which have been declared, but for which no documentation specifications have been declared.")
  (:properties (manual-section targets)))

(def-documentation (function get-target-type)
    (:intro "The \\texttt{get-target-type} function returns the information record of a target type, which should be some subclass of \\texttt{standard-doc-target} or at least implement all of its reader methods.")
  (:callspec (name (:opt no-error)))
  (:params (name "Symbolic name of the target type.")
           (no-error "If nil (the default), the function will raise an error when a symbol which does not name a target type is looked up."))
  (:properties (manual-section targets)))

(def-documentation (function get-doc-hash-of-target-type)
  (:intro
   "The \\texttt{get-doc-hash-of-target-type} function --- \\fbox{FILL IN}")
  (:callspec (tag &key (package package-spec) (spec-type spec-type-name)))
  (:properties (manual-section targets)))

(def-documentation (compiler-macro def-target-type)
  (:intro "The \\texttt{def-target-type} macro defines a new documentation target type.")
  (:callspec (name (&key (class class-name))
                   &body (:key-head docstring-installer (target-name target-spec) (:seq form))))
  (:params (name "Symbolic name of the new target type.")
           (class-name "Class to be used as a record for the stored target type's information.  The default is \\texttt{standard-doc-target}; if another class is used it must support the \\texttt{:name} initarg and \\texttt{docstring-installer} accessor.")
           (docstring-installer "Function which installs a standard Lisp document string for targets of this type."))
  (:properties (manual-section newtargetdef)))


;;; -----------------------------------------------------------------
;;; model

(def-documentation (variable *spec-class*)
  (:intro "The \\texttt{*spec-class*} variable specifies the name of the class used by default as the representation for a documentation specification.")
  (:properties (manual-section control)))

(def-documentation (type doc-spec)
  (:intro "The \\texttt{doc-spec} class is the common superclass of documentation spec classes.  Instances of this class should have methods for the following generic functions: \\raggedright\\texttt{docspec-self}, \\texttt{docspec-target-type}, \\texttt{docspec-tags}, \\texttt{docspec-descriptive}, \\texttt{docspec-intro}, \\texttt{docspec-blurb}, \\texttt{docspec-details}, \\texttt{docspec-params}, \\texttt{docspec-callspecs}, and \\texttt{docspec-deprecated}.")
  (:properties (manual-section model)))

(def-documentation (function docspec-self)
  (:intro "The \\texttt{docspec-self} accessor on documentation objects retrieves the symbolic name of the target of the documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section model)
               (anchor doc-spec)))

(def-documentation (function docspec-target-type)
  (:intro "The \\texttt{docspec-target-type} accessor on documentation objects retrieves the symbolic name of the target type of the documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section model)
               (anchor doc-spec)))

(def-documentation (function docspec-tags)
  (:intro "The \\texttt{docspec-tags} accessor on documentation objects retrieves the tags associated with a particular piece of documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section model)
               (anchor doc-spec)))

(def-documentation (function docspec-descriptive)
  (:intro "The \\texttt{docspec-descriptive} accessor on standard documentation objects retrieves a one-word descriptive string for the target.  Used in FILL IN")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-intro)
  (:intro "The \\texttt{docspec-intro} accessor on standard documentation objects retrieves the introduction element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-blurb)
  (:intro "The \\texttt{docspec-blurb} accessor on standard documentation objects retrieves the short description element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-details)
  (:intro "The \\texttt{docspec-details} accessor on standard documentation objects retrieves the documentation's main body.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-params)
  (:intro "The \\texttt{docspec-params} accessor on standard documentation objects retrieves documentation of the object's parameters.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-callspecs)
  (:intro "The \\texttt{docspec-callspecs} accessor on standard documentation objects retrieves the callspec element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-deprecated)
  (:intro "The \\texttt{docspec-deprecated} accessor on standard documentation objects retrieves a deprecation flag.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function get-label-symbol-value-translation)
  (:intro "Function \\texttt{get-label-symbol-value-translation} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))

(def-documentation (function get-target-type-docspecs)
  (:intro "Function \\texttt{get-target-type-docspecs} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))

(def-documentation (type doc-label)
  (:intro "Type \\texttt{doc-label} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))


;;; -----------------------------------------------------------------
;;; label-model

(def-documentation (function get-label-class)
  (:intro "Function \\texttt{get-label-class} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-labeldef)
  (:intro "Function \\texttt{get-labeldef} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-order-supp-p)
  (:intro "Function \\texttt{get-label-section-order-supp-p} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-title-supp-p)
  (:intro "Function \\texttt{get-label-section-title-supp-p} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function label-values)
  (:intro "Function \\texttt{label-values} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-order)
  (:intro "Function \\texttt{get-label-section-order} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-title)
  (:intro "Function \\texttt{get-label-section-title} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-labeldef)
  (:intro "Function \\texttt{get-labeldef} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-compiled-labeldef)
  (:intro "Function \\texttt{get-compiled-labeldef} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function label-value)
  (:intro "Function \\texttt{label-value} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (type labeled)
  (:intro "Type \\texttt{labeled} --- \\fbox{FILL IN}")
  (:properties (manual-section label-model)))

(def-documentation (function get-label-class)
  (:intro "Function \\texttt{get-label-class} --- \\fbox{FILL IN}")
  (:properties (manual-section get-label-class)))


;;; -----------------------------------------------------------------
;;; elements

(def-documentation (compiler-macro def-element)
  (:intro "Compiler-macro \\texttt{def-element} --- \\fbox{FILL IN}")
  (:callspec (name (new-class &key
                              (class    base-case-name)
                              (package  package-param-name)
                              (spec     spec-param-name)
                              (arg-list arg-list-param-name)
                              (args     args-lambda-list))
                   ((:seq slot-decl)) &body (:seq form)))
  (:properties (manual-section elements))
)

(def-documentation (function compile-string-element)
  (:intro "Function \\texttt{compile-string-element} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function text-element-text)
  (:intro "Function \\texttt{text-element-text} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function element-type-p)
  (:intro "Function \\texttt{element-type-p} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function compile-element)
  (:intro "Function \\texttt{compile-element} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (variable *default-element-class*)
  (:intro "Variable \\texttt{*default-element-class*} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function list-element-env-tag)
  (:intro "Function \\texttt{list-element-env-tag} --- \\fbox{FILL IN}")
  (:callspec (list-element))
  (:properties (manual-section elements)))

(def-documentation (function list-element-options)
  (:intro "Function \\texttt{list-element-options} --- \\fbox{FILL IN}")
  (:callspec (list-element))
  (:properties (manual-section elements)))

(def-documentation (function code-element-string)
  (:intro "Function \\texttt{code-element-string} --- \\fbox{FILL IN}")
  (:callspec (code-element))
  (:properties (manual-section elements)))

(def-documentation (function list-element-specs)
  (:intro "Function \\texttt{list-element-specs} --- \\fbox{FILL IN}")
  (:callspec (list-element))
  (:properties (manual-section elements)))

(def-documentation (function sequence-element-items)
  (:intro "Function \\texttt{sequence-element-items} --- \\fbox{FILL IN}")
  (:callspec (standard-sequence))
  (:properties (manual-section elements)))

(def-documentation (function paragraphlist-element-items)
  (:intro "Function \\texttt{paragraphlist-element-items} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

;;; -----------------------------------------------------------------
;;; output-model

(def-documentation (type output-framework)
  (:intro "Type \\texttt{output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function write-output)
  (:intro "The \\texttt{write-output} function is the main call for producing formatting output documentation.")
  (:callspec (style output-name directory file-name
              &key (index flag) (table-of-contents flag)))
  (:params (style "Object specifying the style for the output.")
           (output-name "Name of the output unit to be formatted.")
           (directory "The directory where the document should be written.")
           (file-name "Root file name for the output.")
           (index "If non-nil and the style supports it, an index will be generated.")
           (table-of-contents "If non-nil and the style supports it, a table of contents will be generated."))
  (:details "Other keyword arguments may be supported by other styles.")
  (:properties (manual-section styles)))

(def-documentation (function get-filename-extension)
  (:intro "The \\texttt{get-filename-extension} function returns a string with the filename extension which should be used for an output file under a particular style.")
  (:callspec (style output-name directory file-name))
  (:details "The arguments are as for \\texttt{write-output}.  For example, this function returns \\texttt{\".tex\"} for a \\LaTeX\\ file."
            "The HTML style does not currently make full use of this style; some instances of \\texttt{\".html\"} are still hardcoded.")
  (:properties (manual-section styles)
               (anchor write-output)))

(def-documentation (function format-docspec)
  (:intro "The \\texttt{format-docspec} function produces output from a documentation specification.")
  (:callspec (stream style spec target-type))
  (:params (stream "Output stream for the result.")
           (style "Object specifying the style for the output.")
           (spec "The documentation spec.")
           (type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods."))
    (:properties (manual-section output-model)))

(def-documentation (type docspec-element)
  (:intro "The \\texttt{docspec-element} class --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function canonicalize-element)
  (:intro "The \\texttt{canonicalize-element} function --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)
               (anchor docspec-element)))

(def-documentation (function format-docspec-element)
  (:intro "The \\texttt{format-docspec-element} function produces output for one docspec element.")
  (:callspec (style target-type element stream))
  (:params (style "Object specifying the style for the output.")
           (target-type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods.")
           (element "The documentation spec.")
           (stream "Output stream for the result."))
  (:properties (manual-section output-model)
               (anchor docspec-element)))

(def-documentation (function format-output-preitem)
  (:intro "Function \\texttt{format-output-preitem} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-postitem)
  (:intro "Function \\texttt{format-output-postitem} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-default-output-contents-sep)
  (:intro "Function \\texttt{format-default-output-contents-sep} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-trailer-docspec)
  (:intro "Function \\texttt{format-output-trailer-docspec} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-leader-docspec)
  (:intro "Function \\texttt{format-output-leader-docspec} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function output-contents-contents)
  (:intro "Function \\texttt{output-contents-contents} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)
               (anchor output-contents)))

(def-documentation (variable *output-nesting-depth*)
  (:intro "Variable \\texttt{*output-nesting-depth*} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-doc-content-items)
  (:intro "Function \\texttt{format-doc-content-items} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-doc-content-item)
  (:intro "Function \\texttt{format-doc-content-item} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-contents-sep)
  (:intro "Function \\texttt{format-output-contents-sep} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function process-standard-output-framework-form)
  (:intro "Function \\texttt{process-standard-output-framework-form} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function get-output-unit-title)
  (:intro "Function \\texttt{get-output-unit-title} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function get-output-unit-author)
  (:intro "Function \\texttt{get-output-unit-author} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function get-output-unit-leader)
  (:intro "Function \\texttt{get-output-unit-leader} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function get-output-unit-trailer)
  (:intro "Function \\texttt{get-output-unit-trailer} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-leader-material)
  (:intro "Function \\texttt{format-output-leader-material} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-leader-sep)
  (:intro "Function \\texttt{format-output-leader-sep} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-leader-title)
  (:intro "Function \\texttt{format-output-leader-title} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-trailer-material)
  (:intro "Function \\texttt{format-output-trailer-material} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (type explicit-doc-element)
  (:intro "Class \\texttt{explicit-doc-element} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function package-exports-p)
  (:intro "Function \\texttt{package-exports-p} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function locate-package-home)
  (:intro "Function \\texttt{locate-package-home} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))


;;; -----------------------------------------------------------------
;;; styles

(def-documentation (type symbol-homing-style)
  (:intro "The \\texttt{symbol-homing-style} style allows documentation to find alternative home packages for symbols.  This mixin allows symbols which are developed as internal symbols in one package, but exported from another interface package.  Style classes inheriting from this mixin should define a method on \\texttt{candidate-home-packages}: if the given symbol is exported from any of the packages returned by this function, it will be documented as being in that package, disregarding any differing result of \\texttt{symbol-package}.")
  (:properties (manual-section styles)))

(def-documentation (function candidate-home-packages)
  (:intro "The \\texttt{candidate-home-packages} function returns a list of packages in which the given specification should first be checked as the exporter of its symbolic name.  Generally, methods of this function are specialized on the style, and ignore the other parameters.")
  (:callspec (style target-type spec))
  (:properties (manual-section styles)
               (anchor symbol-homing-style)))

(def-documentation (type docspec-par-latex-style)
  (:intro "The \\texttt{docspec-par-latex-style} style mixin inserts a paragraph break between consecutive documentation units.")
  (:properties (manual-section styles)))

(def-documentation (type docspec-fancy-header-latex-style)
  (:intro "The \\texttt{docspec-fancy-header-latex-style} style mixin adds fancy headers to the beginning of each docspec.  This class is a subclass of \\texttt{docspec-par-latex-style}.")
  (:properties (manual-section styles)))


;;; -----------------------------------------------------------------
;;; standard-model

(def-documentation (type standard-doc-target)
    (:intro "The \\texttt{standard-doc-target} class implements DefDoc's standard model for the information we store about each target type.")
  (:params (name "Symbol naming the target type.")
           (docstring-installer "Function called to install a docstring.  Functions stored in this slots should take two argument: the symbolic name of the item being documented, and its docspec object."))
  (:properties (manual-section targets)))

(def-documentation (type standard-doc-spec)
  (:intro "The \\texttt{standard-doc-spec} class implements the standard, default class used to represent documentation internally within defdoc.  Objects of this class accept calls to seven accessors \\texttt{docspec-descriptive}, \\texttt{docspec-intro}, \\texttt{docspec-blurb}, \\texttt{docspec-details}, \\texttt{docspec-params}, \\texttt{docspec-callspecs} and \\texttt{docspec-deprecated} as well as to the three accessors of the superclass doc-spec.")
  (:properties (manual-section standard-model)))

(def-documentation (compiler-macro with-unpacked-standard-spec)
    (:intro "The \\texttt{with-unpacked-standard-spec} macro provides deconstruction of \\texttt{standard-doc-spec} objects.")
  (:properties (manual-section control)))

(def-documentation (type standard-sequence)
  (:intro "Type \\texttt{standard-sequence} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type callspec-keyarg)
  (:intro "The \\texttt{callspec-keyarg} class --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-doc-element)
  (:intro "Type \\texttt{standard-doc-element} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-item-to-lines)
  (:intro "Function \\texttt{callspec-item-to-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-default-callspec-block-width)
  (:intro "Function \\texttt{get-default-callspec-block-width} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-paragraph-list)
  (:intro "Class \\texttt{standard-paragraph-list} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-sequence)
  (:intro "Class \\texttt{standard-sequence} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function sequence-element-items)
  (:intro "Function \\texttt{sequence-element-items} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)
               (anchor standard-sequence)))

(def-documentation (type standard-output-framework)
  (:intro "Type \\texttt{standard-output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function doc-label-name)
  (:intro "Function \\texttt{doc-label-name} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))

(def-documentation (type standard-simple-list-environment)
  (:intro "Type \\texttt{standard-simple-list-environment} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-code)
  (:intro "Type \\texttt{standard-code} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-doc-tags)
  (:intro "Function \\texttt{get-doc-tags} --- \\fbox{FILL IN}")
  (:callspec (name target-type))
  (:properties (manual-section control)))

(def-documentation (type standard-itemize)
  (:intro "Type \\texttt{standard-itemize} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)
               (anchor standard-simple-list-environment)))

(def-documentation (function callspec-suffix)
  (:intro "Function \\texttt{callspec-suffix} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-enumerate)
  (:intro "Class \\texttt{standard-enumerate} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)
               (anchor standard-simple-list-environment)))

(def-documentation (type callspec-sequence-of)
  (:intro "Class \\texttt{callspec-sequence-of} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-callspec-sequence-of-repeated)
  (:intro "Function \\texttt{get-callspec-sequence-of-repeated} --- \\fbox{FILL IN}")
  (:callspec (callspec-sequence-item))
  (:properties (manual-section standard-model)
               (anchor callspec-sequence-of)))

(def-documentation (type callspec-optional)
  (:intro "Class \\texttt{callspec-optional} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-callspec-optional-option)
  (:intro "Function \\texttt{get-callspec-optional-option} --- \\fbox{FILL IN}")
  (:callspec (callspec-optional-item))
  (:properties (manual-section standard-model)
               (anchor callspec-optional)))

(def-documentation (type callspec-bag-of)
  (:intro "Class \\texttt{callspec-bag-of} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type callspec-one-of)
  (:intro "Class \\texttt{callspec-one-of} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type callspec-items-holder)
  (:intro "Class \\texttt{callspec-items-holder --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-callspec-holder-items)
  (:intro "Function \\texttt{get-callspec-holder-items} --- \\fbox{FILL IN}")
  (:callspec (callspec-holder))
  (:properties (manual-section standard-model)))

(def-documentation (function get-output-framework)
  (:intro "Function \\texttt{get-output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function output-framework-name)
  (:intro "Function \\texttt{output-framework-name} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)
               (anchor output-framework)))

(def-documentation (function callspec-prefix)
  (:intro "Function \\texttt{callspec-prefix} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-doc-specs)
  (:intro "Function \\texttt{get-doc-specs} --- \\fbox{FILL IN}")
  (:properties (manual-section control)))

(def-documentation (type callspec-keyheaded)
  (:intro "Class \\texttt{callspec-keyheaded} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-callspec-keyheaded-key)
  (:intro "Function \\texttt{get-callspec-keyheaded-key} --- \\fbox{FILL IN}")
  (:callspec (callspec-keyheaded))
  (:properties (manual-section standard-model)))

(def-documentation (function get-callspec-keyheaded-forms)
  (:intro "Function \\texttt{get-callspec-keyheaded-forms} --- \\fbox{FILL IN}")
  (:callspec (callspec-keyheaded))
  (:properties (manual-section standard-model)))

(def-documentation (type standard-doc-label)
  (:intro "The \\texttt{standard-doc-label} class --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-callspec)
  (:intro "Type \\texttt{standard-callspec} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-to-lines)
  (:intro "Function \\texttt{callspec-to-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function process-standard-labeldef-form)
  (:intro "Function \\texttt{process-standard-labeldef-form} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function lower-case-target-name)
  (:intro "Function \\texttt{lower-case-target-name} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function capitalized-target-name)
  (:intro "Function \\texttt{capitalized-target-name} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function standard-callspec-key)
  (:intro "Function \\texttt{standard-callspec-key} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function get-callspec-keyarg-key)
  (:intro "Function \\texttt{get-callspec-keyarg-key} --- \\fbox{FILL IN}")
  (:callspec (callspec-keyarg))
  (:properties (manual-section standard-model)
               (anchor callspec-keyarg)))

(def-documentation (function standard-callspec-body-supp)
  (:intro "Function \\texttt{standard-callspec-body-supp} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-mandatory)
  (:intro "Function \\texttt{standard-callspec-mandatory} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-key-supp)
  (:intro "Function \\texttt{standard-callspec-key-supp} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-optional)
  (:intro "Function \\texttt{standard-callspec-optional} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function get-callspec-keyarg-arg)
  (:intro "Function \\texttt{get-callspec-keyarg-arg} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-callspec)
  (:intro "Function \\texttt{format-standard-docspec-callspec} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function standard-callspec-optional-supp)
  (:intro "Function \\texttt{standard-callspec-optional-supp} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (type macrolist-callspec)
  (:intro "The \\texttt{macrolist-callspec} class --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-param-list-start)
  (:intro "Function \\texttt{format-standard-docspec-param-list-start} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-param-list-item-start)
  (:intro "Function \\texttt{format-standard-docspec-param-list-item-start} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-param-list-item-stop)
  (:intro "Function \\texttt{format-standard-docspec-param-list-item-stop} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-param-list-stop)
  (:intro "Function \\texttt{format-standard-docspec-param-list-stop} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-param-list-item)
  (:intro "Function \\texttt{format-standard-docspec-param-list-item} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)))

(def-documentation (function standard-callspec-body)
  (:intro "Function \\texttt{standard-callspec-body} --- \\fbox{FILL IN}")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model)
               (anchor standard-callspec)))

(def-documentation (function format-standard-docspec-param-list)
  (:intro "Function \\texttt{format-standard-docspec-param-list} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function format-standard-docspec-literal-text)
  (:intro "Function \\texttt{format-standard-docspec-literal-text} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))


;;; -----------------------------------------------------------------
;;; plaintext

(def-documentation (type standard-docstring-style)
    (:intro "The \\texttt{standard-docstring-style} class is a standard implementation of the default style for docstring generation.")
  (:properties (manual-section plaintext-model)))

(def-documentation (type standard-plain-text)
  (:intro "Type \\texttt{standard-plain-text} --- \\fbox{FILL IN}")
  (:properties (manual-section plaintext-model)))

(def-documentation (function indent-with)
    (:callspec (lines length))
  (:properties (manual-section plaintext-model)))

(def-documentation (function adjoin-blocks)
    (:callspec (lines length))
  (:properties (manual-section plaintext-model)))

(def-documentation (function indent-by)
    (:callspec (lines length))
  (:properties (manual-section plaintext-model)))

(def-documentation (function bracket-with)
    (:callspec (lines prefix suffix))
  (:properties (manual-section plaintext-model)))

(def-documentation (function width)
    (:callspec (lines))
  (:properties (manual-section plaintext-model)))

(def-documentation (function whitespace-p)
    (:callspec (char))
  (:properties (manual-section plaintext-model)))

(def-documentation (function flow)
    (:callspec (formatter artifacts max))
  (:properties (manual-section plaintext-model)))

(def-documentation (function output-lines)
  (:intro "Function \\texttt{output-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section plaintext-model)))

(def-documentation (variable *docstring-style*)
  (:intro "The \\texttt{*docstring-style*} variable specifies the style used when generating docstrings.")
  (:properties (manual-section plaintext-model)))


;;; -----------------------------------------------------------------
;;; latex

(def-documentation (variable *latex-verbatim-width*)
    (:intro "The \\texttt{*latex-verbatim-width*} variable specifies the maximum line length in verbatim mode")
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-spec-latex)
  (:intro "The \\texttt{write-spec-latex} function writes a single document spec to a \\LaTeX\\ source file.")
  (:callspec (name usage &key
                   (style style)
                   (directory pathname)
                   (file filename-string)))
  (:params (name (:latex "Symbol naming the entity to be documented."))
           (usage (:latex "Symbol naming the target type of the entity, e.g.\ \\texttt{function}, \\texttt{type}, etc."))
           (style (:latex "Style class to be applied for this output; the default is \\texttt{latex-style}."))
           (directory (:latex "Directory where the output file should be written; the default is the value of \\texttt{*defdoc-latex-default-directory*}."))
           (file (:latex "File to be created or overwritten with this content; by default the result of a call to \\texttt{get-latex-output-file-name} is used.")))
  (:properties (manual-section latex)))

(def-documentation (variable *latex-full-package-item-header-macro*)
    (:intro "The \\texttt{*latex-full-package-item-header-macro*} variable is a string with the \\LaTeX\\ macro used as a section header in the \\texttt{full-package} style.")
  (:properties (manual-section latex-style-model)))

(def-documentation (variable *defdoc-latex-default-directory*)
  (:intro "The \\texttt{*defdoc-latex-default-directory*} variable is a string naming the directry into which generated \\LaTeX\\ output should be written.")
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-package-specs-latex)
  (:intro "The \\texttt{write-package-specs-latex} function generates \\LaTeX\\ source files for all docspecs for entities named by symbols in a particular package.")
  (:callspec (package-spec &key (style name) (package-style name)
                           (include-doctypes ( (:seq doctype) ))
                           (directory pathname) (echo callback)))
  (:params (package-spec (:latex "Specifier for the package for which output is to be generated."))
           (style (:latex "Style class to be applied for this output; the default is \\texttt{latex-style}."))
           (package-style (:latex "When non-nil, indicated that a \\LaTeX\\ file documenting the package itself should be written, and names the style class to be applied for that output file.  The default is \\texttt{t}, which indicates that the value of the \\texttt{:style} argument should be used here as well."))
           (doctype (:latex "This list of symbols naming spec type selects additional documentation to be written: when a type is given here, all docspecs are that type will be written \\emph{whether or not the specs are attached to entities named by symbols in the package targeted by this call}."))
           (directory (:latex "Directory where the output files should be written; the default is the value of \\texttt{*defdoc-latex-default-directory*}."))
           (callback (:latex "The echo callback is invoked for each item written; the call is passed keyword arguments \\texttt{:name} and \\texttt{:type} for the symbols naming respectively the entity name and doctype.  The callback is useful for providing feedback on long processes to the user.")))
  (:properties (manual-section latex)))

(def-documentation (function process-latex-document)
  (:intro "Function \\texttt{process-latex-document} is a utility function which runs \\LaTeX\\ and associated tools on a particular file.")
  (:callspec (directory file-name-root &key (bibtex flag) (index flag)))
  (:params (directory (:latex "Directory holding the \\LaTeX\\ source file."))
           (file-name-root (:latex "Root file name (i.e.\\ dropping the ``\\texttt{.tex}'' suffix) of the \\LaTeX\\ source file."))
           (bibtex (:latex "If non-nil, indicates that Bib\\TeX\\ should be used to generate a bibliography."))
           (index (:latex "If non-nil, indicates that \\texttt{makeindex} should be used to generate an index.")))
  (:details "Although \\texttt{process-latex-document} does not attempt to parse the output of the various tools, it will re-run \\LaTeX\\ as it anticipates to be necessary to resolve cross-references, which may result in running the tool more times than is strictly necessary."
            "Invocation of external programs is not included in the Common Lisp specification, and varies by platform.  Thus, this function is implementated on a platform-by-platform basis.  At this writing, it is known to work on Allegro and Steel Bank Common Lisps only.")
  (:properties (manual-section latex)))

(def-documentation (function write-latex-output)
  (:intro "Function \\texttt{write-latex-output} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-doctype-latex)
  (:intro "The \\texttt{write-doctype-latex} function generates \\LaTeX\\ source files for all docspecs of a certain target type.")
  (:callspec (doctype &key (style style) (directory pathname) (echo callback)))
  (:params (doctype (:latex "Symbol naming the target type of the entities to be written, e.g.\ \\texttt{function} or \\texttt{type}."))
           (style (:latex "Style class to be applied for this output; the default is \\texttt{latex-style}."))
           (directory (:latex "Directory where the output files should be written; the default is the value of \\texttt{*defdoc-latex-default-directory*}."))
           (callback (:latex "The echo callback is invoked for each item written; the call is passed keyword arguments \\texttt{:name} and \\texttt{:type} for the symbols naming respectively the entity name and doctype.  The callback is useful for providing feedback on long processes to the user.")))
  (:properties (manual-section latex)))

(def-documentation (function get-element-for-docspec-format)
  (:intro "Function \\texttt{get-element-for-docspec-format} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (type latex-style)
  (:intro (:latex "The \\texttt{latex-style} class is a base class for \\LaTeX\\ document generation."))
  (:properties (manual-section styles)))

(def-documentation (type plaintext-style)
  (:intro (:latex "The \\texttt{plaintext-style} class is a base class for generating text documents."))
  (:properties (manual-section styles)))

(def-documentation (type full-package-latex-style-mixin)
  (:intro "Type \\texttt{full-package-latex-style-mixin} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (type standard-latex)
  (:intro "Type \\texttt{standard-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (function latex-element-latex)
  (:intro "Function \\texttt{latex-element-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)
               (anchor standard-latex)))

(def-documentation (function package-list-overall-header)
  (:intro "Function \\texttt{package-list-overall-header} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (function get-latex-output-file-name)
  (:intro "Function \\texttt{get-latex-output-file-name} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))

(def-documentation (type package-list-latex-mixin)
  (:intro "Type \\texttt{package-list-latex-mixin} --- \\fbox{FILL IN}")
  (:properties (manual-section latex-style-model)))


;;; -----------------------------------------------------------------
;;; HTML

(def-documentation (compiler-macro with-div-wrapper)
  (:intro (:latex "The \\texttt{with-div-wrapper} macro \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (compiler-macro with-span-wrapper)
  (:intro (:latex "The \\texttt{with-span-wrapper} macro \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (type html-style)
  (:intro (:latex "The \\texttt{html-style} class is a base class for HTML document generation.  Note that \\LaTeX's \\texttt{makeindex} program can become confused if a directory has the same name as a \\LaTeX\\ source file: so a file \\texttt{mydoc.tex} and a generated directory of HTML files \\texttt{mydoc} should not actually be given the same name."))
  (:properties (manual-section styles)))

(def-documentation (function write-html-output-index-page)
  (:intro "The \\texttt{write-html-output-index-page} function \\fbox{FILL IN}")
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-header-block)
  (:intro "The \\texttt{format-html-output-index-page-header-block} function \\fbox{FILL IN}")
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-headers)
  (:intro "The \\texttt{format-html-output-index-page-headers} function \\fbox{FILL IN}")
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-body)
  (:intro (:latex "The \\texttt{format-html-output-index-page-body} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function traverse-and-write-output-pages)
  (:intro (:latex "The \\texttt{traverse-and-write-output-pages} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function get-html-disambiguator)
  (:intro (:latex "The \\texttt{get-html-disambiguator} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-content-link)
  (:intro (:latex "The \\texttt{format-content-link} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function get-content-link-filename)
  (:intro (:latex "The \\texttt{get-content-link-filename} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-content-anchor)
  (:intro (:latex "The \\texttt{format-content-anchor} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))

(def-documentation (function html-free-paragraph-docspec-element)
  (:intro (:latex "The \\texttt{html-free-paragraph-docspec-element} function \\fbox{FILL IN}"))
  (:properties (manual-section html-style-model)))


;;; -----------------------------------------------------------------
;;; deprecated

(def-documentation (compiler-macro def-doc-tag)
  (:intro "The \\texttt{def-doc-tag} macro is deprecated.")
  (:properties (manual-section deprecated)))

(def-documentation (function tag-sort)
  (:intro "Function \\texttt{tag-sort} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

(def-documentation (function format-tag)
  (:intro "Function \\texttt{format-tag} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-group-header)
  (:intro "Function \\texttt{package-list-group-header} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-entry)
  (:intro "Function \\texttt{package-list-entry} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

;;; -----------------------------------------------------------------
;;; Overall package documentation.

(def-documentation (package :defdoc)
    (:blurb "Container for high-level use of the structured documentation manager DefDoc.")
  (:descriptive "Defdoc"))

;;; -----------------------------------------------------------------
;;; Issue warnings if anything is not documented.
(ensure-api-documentation :defdoc)
(ensure-api-documentation :defdoc-control-api)
