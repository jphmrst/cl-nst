;;; File documentation.lisp
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
  (:intro "A "
          (:lisp doc-element :plain)
          " element should have one other element in the list, a string of plain text, with no backend-specific formatting.")
 (:callspec (string)))

(def-documentation (doc-element :latex)
  (:intro "A "
          (:lisp doc-element :latex)
          " element should have one other element in the list, a string of "
          (:latex-name)
          " source.")
 (:callspec (string)))

(def-documentation (doc-element :paragraphs)
  (:intro "A "
          (:lisp doc-element :paragraphs)
          " element represents a sequence of paragraphs corresponding to the rest of the list.")
 (:callspec ((:seq string-or-docspec))))

(def-documentation (doc-element :seq)
  (:intro "A "
          (:lisp doc-element :seq)
          " element will combine the rest of the element's list into a single paragraph.")
 (:callspec ((:seq string-or-docspec))))

(def-documentation (doc-element :code)
  (:intro "A "
          (:lisp doc-element :code)
          " element should have one other element in the list, a string of plain text, possibly including line breaks, which will be rendered exactly as it is as an example of code.")
 (:callspec (string)))

(def-documentation (doc-element :inline)
  (:intro "An "
          (:lisp doc-element :inline)
          " element is used for short single-line code or similar snippets.")
 (:callspec (string)))

(def-documentation (doc-element :itemize)
  (:intro "The list items after an "
          (:lisp doc-element :itemize)
          " element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of an itemized list.")
 (:callspec (() (:seq string-or-docspec))))

(def-documentation (doc-element :enumerate)
  (:intro "The list items after an "
          (:lisp doc-element :enumerate)
          " element (and its second argument, which specifies optional keyword arguments about the list itself) are taken as the contents of a numbered list.")
                (:callspec (() (:seq string-or-docspec))))

(def-documentation (doc-element :latex-name)
  (:intro "The " (:lisp doc-element :latex-name) " element denotes the name "
          (:latex-name) " with any available special rendering.")
  (:callspec ()))

(def-documentation (doc-element :output-set)
  (:intro "The " (:lisp doc-element :output-set) " element specifies the name of a unit of output to be included in the document.")
  (:callspec (name &key (style style-name))))

(def-output-class doc-elements
    (collect-target-type 'doc-element))

;;; -----------------------------------------------------------------
;;; Special callspec list headers

(def-target-type callspec-special (:symbol-definition-nocheck t))

(def-documentation (callspec-special :seq)
  (:intro "The " (:lisp callspec-special :seq)
          " symbol indicates that the remaining specifiers of the list should be repeated in sequence.")
  (:details "In the above example,"
            (:code "  (:seq call-sequence)")
            "would be rendered under the standard document style as:"
            (:code "  call-sequence ... call-sequence")
            "and"
            (:code "  (:seq (name (:seq val)))")
            "would be rendered as:"
            (:code "  (name val ... val) ... (name val ... val)")))

(def-documentation (callspec-special :bag)
  (:intro "The " (:lisp callspec-special :bag)
          " symbol specifies usage of the following specifiers once or more each in any order.")
  (:details "For example,"
            (:code "  (:bag simple (less simple)
        (rather (:seq less) simple))")
            "could be rendered as:"
            (:code " [ simple
    | (less simple)
    | (rather less ... less simple ]*")))

(def-documentation (callspec-special :alt)
  (:intro "The " (:lisp callspec-special :alt)
          " symbol indicates that exactly one of the list's specifiers should be satisfied, and the rest ignored.")
  (:details "For example,"
            (:code "  (:alt :light :medium :dark)")
            "would be rendered as:"
            (:code " [ :light | :medium | :dark ]")))

(def-documentation (callspec-special :opt)
  (:intro "The " (:lisp callspec-special :opt)
          " symbol specifies optional forms.")
  (:details (:code "  (:opt (name (:seq val)))")
            "would be rendered as:"
            (:code "  [ (name val ... val) ]")))

(def-documentation (callspec-special :key-head)
  (:intro "The " (:lisp callspec-special :key-head)
          " symbol specifies a list headed by a keyword argument, and the form of the other values in that list.")
  (:details "In the example above,"
            (:code "  (:key-head blurb string-or-docspec)")
            "would be rendered as:"
            (:code "  (:blurb string-or-docspec)")
            "The "
            (:lisp callspec-special :key-head)
            " specifier removes any ambiguity in the callspecs of macro forms such as the body forms of a "
            (:lisp compiler-macro defclass)
            "."
            (:code "  (:key-head seq item)")
            "would render as:"
            (:code "  (:seq item)")))

(def-output-class callspec-specials
    (collect-target-type 'callspec-special))

;;; -----------------------------------------------------------------
;;; Main macros


(def-documentation (compiler-macro def-documentation)
  (:intro "The " (:lisp compiler-macro def-documentation)
          " macro attaches user documentation to a function, macro, or other documentable entity.")
  (:callspec (name &body
                   (:opt (:key-head blurb string-or-docspec))
                   (:opt (:key-head :intro string-or-docspec))
                   (:opt (:key-head :params (:seq (name string-or-docspec))))
                   (:opt (:key-head :callspec (:seq call-sequence)))
                   (:opt (:key-head :details string-or-docspec))
                   (:opt (:key-head :properties (:seq (prop-name value))))))
  (:params (string-or-docspec
            (:paragraphs
             (:seq "Actual documentation can be given as either a "
                   (:emph "docspec element")
                   ", or a list with a recognized keyword as its first value, or as a plain string.  The standard docspec elements are:")
             (:output-set doc-elements)
             "Otherwise, lists in the callspec are taken to be a structured list argument, as for a macro lambda-list."))
           (full "Discussion section of the item's full documentation.  Generally, will be displayed after the introduction, usage specifications and parameter list.")
           (properties "List of property-value pairs to be associated with this documentation.  The property names should have be defined with the "
                       (:lisp compiler-macro def-property-label)
                       " macro."))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro def-bare-string-element-tag)
  (:intro "The " (:lisp compiler-macro def-bare-string-element-tag)
          " macro specifies how an unannotated string in a documentation spec should be interpreted.")
  (:callspec (tag &key (package name-or-package) (spec-type class-name)))
  (:params (tag "Keyword tag within which the bare string should be wrapped.")
           (name-or-package "Names the package within which this default applies.")
           (class-name "Allows a particular documentation specification base type to qualify the default value."))
  (:details "By default, a string will be interpreted as plain text.  In other words, the generic function for which this macro encapsulates a method declaration returns, by default "
            (:lisp doc-element :plain)
            ", so that a bare "
            (:lisp type string)
            " is wrapped as:"
            (:code "  (:plain string)"))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro ensure-api-documentation)
  (:intro "DefDoc provides "
          (:lisp compiler-macro ensure-api-documentation)
          " as a top-level form to check that all symbols in a package are provided documentation.")
  (:callspec (package &key (react flag) (error flag) (warning flag)
                           (internal flag) (defdoc-only flag)))
  (:params (package "The package to be checked")
           (error (:latex
                   "If non-nil, raise an error on an undocumented symbol."))
           (warning "If the "
                    (:lisp keyword :error)
                    " argument is non-nil, then this argument is ignored.  Otherwise, if non-nil a "
                    (:lisp type warning)
                    " (as opposed to a "
                    (:lisp type style-warning)
                    " if "
                    (:lisp symbol null)
                    "), is raised on an undocumented symbol.")
           (internal "If non-nil, all symbols of the package are examined, not just the exported symbols.")
           (defdoc-only "If non-nil, only documentation provided via DefDoc is considered ``passing'' for this check.  Otherwise, the check will accept regular Lisp docstrings."))
  (:properties (manual-section docspecs)))

(def-documentation (compiler-macro def-property-label)
  (:intro "The " (:lisp compiler-macro def-property-label)
          " macro declares a label for use in the "
          (:lisp keyword :properties)
          " of a documentation spec.")
  (:callspec (label-name () &body (:opt (:key-head :default-subsort name))))
  (:properties (manual-section label-use)))

(def-documentation (compiler-macro def-label-config)
  (:intro "The " (:lisp compiler-macro def-label-config)
          " macro specifies information associated with particular uses, especially particular outputs, of a property label.")
  (:callspec ((&key (label label-name)
                    (style style)
                    (output-framework output-name)
                    (package package))
              &body (:seq (value &key (title title-spec-element)
                                 (order symbol-list)))))
  (:params (label-name "The name of a label to be configured.  Note that you do "
                       (:emph "not")
                       " need to specify label: this macro translates to internal method declarations; omitting a label could be used, for example, to give names to certain values of all label in the context of a particular style.")
           (style "If provided, names the style class to which this configuration should apply.")
           (output-name "If provided, the output unit name to which this configuration should apply.")
           (package "The package in which the value and label names given in this form should be interned.  By default, they are left in the current package.  Forward declarations cannot be used here; the package will be referenced when the "
                    (:lisp compiler-macro def-label-config)
                    " macro is expanded.")
           (value "Designates the property value with which certain values should be associated.")
           (title-spec-element "Spec element corresponding to the section title associated with a particular label value.")
           (symbol-list "Order of display associated with a particular label value."))
  (:properties (manual-section label-use)))


;;; -----------------------------------------------------------------
;;; outspec

(def-documentation (compiler-macro def-output-class)
  (:intro "The " (:lisp compiler-macro def-output-class)
          " macro defines the contents of an output document.")
  (:callspec ((output-class-name &key (class base-class)
                                 (title title-spec)
                                 (author author-spec))
              &body
              (:seq collection-form)))
  ;; (:details )
  (:params (output-class-name
            "The name of the class being created for this output specification")
           (base-class "Base class for the definition, by default "
                       (:lisp type output-contents) ".")
           (title-spec "The title of the document component.")
           (author-spec "The author of the document component.")
           (collection-form "One or more calls to the "
                            (:lisp symbol collect-)
                            " functions below."))
  (:properties (manual-section outspec)))

(def-documentation (type output-contents)
  (:intro "The " (:lisp type output-contents)
          " class --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (type grouped-output-contents)
  (:intro "The " (:lisp type grouped-output-contents)
          " class --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function format-doc)
  (:intro "The "
          (:lisp function format-doc)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function get-grouped-output-group)
  (:intro "The " (:lisp function get-grouped-output-group)
          " function --- "
          (:fill-in))
  (:callspec (grouped-output))
  (:properties (manual-section output-model)))

(def-documentation (function get-grouped-output-labeldef)
  (:intro "The " (:lisp function get-grouped-output-labeldef)
          " function --- "
          (:fill-in))
  (:callspec (grouped-output))
  (:properties (manual-section output-model)))

(def-documentation (function get-included-outputset-style)
  (:intro "The " (:lisp function get-included-outputset-style)
          " function --- "
          (:fill-in))
  (:callspec (outer-style inner-style contained))
  (:properties (manual-section output-model)))

(def-documentation (compiler-macro collect-groups-by-label)
  (:intro "Macro "
          (:lisp compiler-macro collect-groups-by-label)
          " forms groups of output units by grouping according to property label values.")
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
           (boolean "If "
                    (:lisp symbol nil)
                    " indicates that none of the document specifications accumulated by the "
                    (:lisp symbol collection-form)
                    "s should be omitted from the groups generated by this form.  For example, if some specs' label values are not included in the "
                    (:lisp keyword :groups)
                    " list, and this flag is set, then the macro will issue a warning.")
           (collection-form "One or more calls to "
                            (:lisp symbol collect-)
                            " functions which will be used to select the documentation specs to be grouped."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-doc)
  (:intro "Macro "
          (:lisp compiler-macro collect-doc)
          " accumulates a single output element from a static document specification.  This macro allows literal text to be inserted into an output unit.")
  (:callspec (options (:seq form)))
  (:params (form "The forms are assembled into a document element.")
           (options "In this version, no options are recognized and this argument is ignored."))
  (:details (:seq "For example:")
            (:code "(collect-doc ()
  (:latex \"And speaking of which:\")
  (:enumerate ()
    (:latex \"This stuff is cool.\")
    (:latex \"This next stuff is confusing.\")))"))
  (:properties (manual-section outspec)))

(def-documentation (function collect-target-type)
  (:intro "The "
          (:lisp function collect-target-type)
          " function aggregates documentation specifications for the "
          (:lisp compiler-macro def-output-class)
          " macro according to the target type to which the specifications is attached --- all functions, all compiler macros, or more typically, all of a particular user-defined target type.")
  (:params (target-name "The target type of interest.")
           (filter (:seq
                    "The filters in "
                    (:lisp symbol collect-)
                    " functions specify further conditions for inclusion in the "
                    (:lisp symbol collect)
                    "ed result.  The filter itself is a list of keyword parameters.  Currently the "
                    (:lisp symbol collect)
                    " functions support one filter:"
                    (:itemize ()
                              (:seq "The "
                                      (:lisp keyword :package)
                                      " filter collects only specifications documenting symbols in the given package.")))))
  (:callspec (target-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-exported-symbols)
  (:intro "The "
          (:lisp function collect-exported-symbols)
          " function collects documentation from the symbols exported from a particular package.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for "
                   (:lisp function collect-target-type)
                   "."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-symbols)
  (:intro "The " (:lisp compiler-macro collect-symbols)
          " macro simply accumulates the documentation of the named symbols.")
  (:callspec (package symbol-list (:seq filter)))
  (:params (package "The package in which the symbols to be collected are interned.  Note that this package need not necessarily exist for the "
                    (:lisp compiler-macro def-output-class)
                    " to be compiled; DefDoc will not attempt to access the package until the output unit is instantiated.")
           (symbol-list "Symbols to be collected.")
           (filter "As for "
                   (:lisp function collect-target-type)
                   "."))
  (:properties (manual-section outspec)))

(def-documentation (function collect-documented-symbols)
  (:intro "The "
          (:lisp function collect-documented-symbols)
          " function collects documentation from all symbols in the package, exported or not, which have documentation explicitly declared for them.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for "
                   (:lisp function collect-target-type)
                   " ."))
  (:properties (manual-section outspec)))

(def-documentation (function collect-all-symbols)
  (:intro "The "
          (:lisp function collect-all-symbols)
          " function collects documentation for all symbols in a package.")
  (:callspec (package-name (:seq filter)))
  (:params (package-name "The package whose symbols' documentation specs are collected.")
           (filter "As for "
                   (:lisp function collect-target-type)
                   "."))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-output)
  (:intro "The "
          (:lisp compiler-macro collect-output)
          " function allows included output class declarations to be nested within the including unit.")
  (:callspec (((:opt output-class-name)
               &key (class base-class) (title title-spec) (author author-spec))
              &body
              (:seq collection-form))
             (output-class-name &body (:seq collection-form)))
  (:details "Arguments are just as for "
            (:lisp compiler-macro def-output-class)
            "; this call essentially just expands to a call to "
            (:lisp function collect-named-output)
            " with the new "
            (:lisp compiler-macro def-output-class)
            " prepended to the present one.  If no "
            (:lisp symbol output-class-name)
            " is present, the result of a call to "
            (:lisp function gensym)
            " is used.")
  (:properties (manual-section outspec)))

(def-documentation (function collect-named-output)
  (:intro "The "
          (:lisp function collect-named-output)
          " function includes the instantiation of a named output unit class as a component of another output unit.")
  (:callspec (output-unit-name))
  (:params (output-unit-name "The output unit class to be included."))
  (:details "DefDoc does "
            (:emph "not")
            " check for (but is vulnerable to) circular inclusion of output units.")
  (:properties (manual-section outspec)))

;;; -----------------------------------------------------------------
;;; control

(def-documentation (function string-implicit-symbol-head)
  (:intro "The generic function "
          (:lisp function string-implicit-symbol-head)
          " directs how a bare string is converted into a documentation spec element.")
  (:callspec (package doc-spec string))
  (:properties (manual-section control)))

(def-documentation (function get-doc-spec)
  (:intro "The " (:lisp function get-doc-spec)
          " function retrieves a docspec for an item of a particular target type.")
  (:callspec (name target-type))
  (:params (name "Symbol naming the item.")
           (target-type "The target-type of the spec."))
  (:properties (manual-section control)))

(def-documentation (function get-spec-class)
  (:intro "The "
          (:lisp function get-spec-class)
          " function --- "
          (:fill-in))
  (:callspec (package name form-list))
  (:properties (manual-section control)))

(def-documentation (function compile-spec)
  (:intro "The "
          (:lisp function compile-spec)
          " function --- "
          (:fill-in))
  (:properties (manual-section control)))

(def-documentation (setf get-doc-spec)
  (:blurb "Used with "
          (:lisp symbol setf)
          ", "
          (:lisp setf get-doc-spec)
          " sets the docspec for an item of a particular target type.")
  (:properties (manual-section control)))


;;; -----------------------------------------------------------------
;;; targets

(def-documentation (function get-doc-target-types)
  (:intro "The " (:lisp function get-doc-target-types)
          " function returns a list of sybolic names of the target types currently in use.")
  (:callspec ((:opt symbol)))
  (:params (symbol "If provided, then the function returns all target types for which the given symbol has a documentation instance."))
  (:details "The result does not include target types which have been declared, but for which no documentation specifications have been declared.")
  (:properties (manual-section targets)))

(def-documentation (function get-target-type)
  (:intro "The " (:lisp function get-target-type)
          " function returns the information record of a target type, which should be some subclass of "
          (:lisp type standard-doc-target)
          " or at least implement all of its reader methods.")
  (:callspec (name (:opt no-error)))
  (:params (name "Symbolic name of the target type.")
           (no-error "If nil (the default), the function will raise an error when a symbol which does not name a target type is looked up."))
  (:properties (manual-section targets)))

(def-documentation (function get-doc-hash-of-target-type)
  (:intro "The " (:lisp function get-doc-hash-of-target-type)
          " function --- "
          (:fill-in))
  (:callspec (tag &key (package package-spec) (spec-type spec-type-name)))
  (:properties (manual-section targets)))

(def-documentation (compiler-macro def-target-type)
  (:intro "The " (:lisp compiler-macro def-target-type)
          " macro defines a new documentation target type.")
  (:callspec (name (&key (class class-name))
                   &body (:key-head docstring-installer (target-name target-spec) (:seq form))))
  (:details "Separate target types are useful, for example, when documenting uses of literal symbols in S-expressions.  Such documentation can be aggregated into output sets using " (:lisp function collect-target-type) ", and as a documentation element using " (:lisp doc-element :output-set) ".")
  (:params (name "Symbolic name of the new target type.")
           (class-name "Class to be used as a record for the stored target type's information.  The default is "
                       (:lisp type standard-doc-target)
                       "; if another class is used it must support the "
                       (:lisp keyword :name)
                       " initarg and "
                       (:lisp function docstring-installer)
                       " accessor.")
           (docstring-installer "Function which installs a standard Lisp document string for targets of this type."))
  (:properties (manual-section newtargetdef)))


;;; -----------------------------------------------------------------
;;; model

(def-documentation (variable *spec-class*)
  (:intro "The " (:lisp variable *spec-class*)
          " variable specifies the name of the class used by default as the representation for a documentation specification.")
  (:properties (manual-section control)))

(def-documentation (type doc-spec)
  (:intro "The " (:lisp type doc-spec)
          (:latex " class is the common superclass of documentation spec classes.  Instances of this class accept two accessors, ")
          (:lisp function docspec-self)
          " and "
          (:lisp function docspec-target-type)
          ".")
  (:properties (manual-section model)))

(def-documentation (function docspec-self)
  (:intro "The " (:lisp function docspec-self)
          " accessor on documentation objects retrieves the symbolic name of the target of the documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section model)
               (anchor doc-spec)))

(def-documentation (function docspec-target-type)
  (:intro "The " (:lisp function docspec-target-type)
          " accessor on documentation objects retrieves the symbolic name of the target type of the documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section model)
               (anchor doc-spec)))

(def-documentation (function docspec-tags)
  (:intro "The " (:lisp function docspec-tags)
          " accessor on documentation objects retrieves the tags associated with a particular piece of documentation.")
  (:callspec (doc-spec))
  (:properties (manual-section deprecated)))

(def-documentation (function docspec-descriptive)
  (:intro "The " (:lisp function docspec-descriptive)
          " accessor on standard documentation objects retrieves a one-word descriptive string for the target.  This field is useful for extracting section headers, such as by the " (:lisp type package-list-latex-mixin) " style.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-intro)
  (:intro "The " (:lisp function docspec-intro)
          " accessor on standard documentation objects retrieves the introduction element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-blurb)
  (:intro "The " (:lisp function docspec-blurb)
          " accessor on standard documentation objects retrieves the short description element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-details)
  (:intro "The " (:lisp function docspec-details)
          " accessor on standard documentation objects retrieves the documentation's main body.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-params)
  (:intro "The " (:lisp function docspec-params)
          " accessor on standard documentation objects retrieves documentation of the object's parameters.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-callspecs)
  (:intro "The " (:lisp function docspec-callspecs)
          " accessor on standard documentation objects retrieves the callspec element.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function docspec-deprecated)
  (:intro "The " (:lisp function docspec-deprecated)
          " accessor on standard documentation objects retrieves a deprecation flag.")
  (:callspec (standard-doc-spec))
  (:properties (manual-section standard-model)
               (anchor standard-doc-spec)))

(def-documentation (function get-label-symbol-value-translation)
  (:intro "The "
          (:lisp function get-label-symbol-value-translation)
          " function --- "
          (:fill-in))
  (:properties (manual-section control)))

(def-documentation (function get-target-type-docspecs)
  (:intro "The "
          (:lisp function get-target-type-docspecs)
          " function --- "
          (:fill-in))
  (:properties (manual-section control)))

(def-documentation (type doc-label)
  (:intro "Type "
          (:lisp function doc-label)
          " --- "
          (:fill-in))
  (:properties (manual-section control)))


;;; -----------------------------------------------------------------
;;; label-model

(def-documentation (function get-label-class)
  (:intro "The "
          (:lisp function get-label-class)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-labeldef)
  (:intro "The "
          (:lisp function get-labeldef)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-order-supp-p)
  (:intro "The "
          (:lisp function get-label-section-order-supp-p)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-title-supp-p)
  (:intro "The "
          (:lisp function get-label-section-title-supp-p)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function label-values)
  (:intro "The "
          (:lisp function label-values)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-order)
  (:intro "The "
          (:lisp function get-label-section-order)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-label-section-title)
  (:intro "The "
          (:lisp function get-label-section-title)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-labeldef)
  (:intro "The "
          (:lisp function get-labeldef)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-compiled-labeldef)
  (:intro "The "
          (:lisp function get-compiled-labeldef)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function label-value)
  (:intro "The "
          (:lisp function label-value)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (type labeled)
  (:intro "Type "
          (:lisp function labeled)
          " --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function get-label-class)
  (:intro "The "
          (:lisp function get-label-class)
          " function --- "
          (:fill-in))
  (:properties (manual-section get-label-class)))


;;; -----------------------------------------------------------------
;;; elements

(def-documentation (compiler-macro def-element)
  (:intro "Compiler-macro "
          (:lisp compiler-macro def-element)
          " --- "
          (:fill-in))
  (:callspec (name (new-class &key
                              (class    base-case-name)
                              (package  package-param-name)
                              (spec     spec-param-name)
                              (arg-list arg-list-param-name)
                              (args     args-lambda-list))
                   ((:seq slot-decl)) &body (:seq form)))
  (:properties (manual-section standard-model-elements))
)

(def-documentation (function compile-string-element)
  (:intro "The "
          (:lisp function compile-string-element)
          " function --- "
          (:fill-in))
  (:properties (manual-section elements)))

(def-documentation (function spaceheaded-element)
  (:intro "The "
          (:lisp function spaceheaded-element)
          " function --- "
          (:fill-in))
  (:properties (manual-section elements)))

(def-documentation (function element-type-p)
  (:intro "The "
          (:lisp function element-type-p)
          " function --- "
          (:fill-in))
  (:properties (manual-section elements)))

(def-documentation (function compile-element)
  (:intro "The "
          (:lisp function compile-element)
          " function --- "
          (:fill-in))
  (:properties (manual-section elements)))

(def-documentation (variable *default-element-class*)
  (:intro "Variable "
          (:lisp variable *default-element-class*)
          " --- "
          (:fill-in))
  (:properties (manual-section elements)))

;;; -----------------------------------------------------------------
;;; output-model

(def-documentation (type output-framework)
  (:intro "Type "
          (:lisp type output-framework)
          " --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function write-output)
  (:intro "The " (:lisp function write-output)
          " function is the main call for producing formatting output documentation.")
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
  (:intro "The " (:lisp function get-filename-extension)
          " function returns a string with the filename extension which should be used for an output file under a particular style.")
  (:callspec (style output-name directory file-name))
  (:details "The arguments are as for "
            (:lisp function write-output)
            ".  For example, this function returns "
            (:inline "\".tex\"")
            " for a "
            (:latex-name)
            " file."
            "The HTML style does not currently make full use of this function; some instances of "
            (:inline "\".html\"")
            " are still hardcoded.")
  (:properties (manual-section styles)
               (anchor write-output)))

(def-documentation (function format-docspec)
  (:intro "The " (:lisp function format-docspec)
          " function produces output from a documentation specification.")
  (:callspec (stream style spec target-type))
  (:params (stream "Output stream for the result.")
           (style "Object specifying the style for the output.")
           (spec "The documentation spec.")
           (type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods."))
    (:properties (manual-section output-model)))

(def-documentation (type docspec-element)
  (:intro "The " (:lisp type docspec-element)
          " class is the common superclass of the individual elements of a documentation specification: snippets of plain text or " (:latex-name) ", sequences or paragraph lists composed of other elements, and so forth.")
  (:properties (manual-section model)))

(def-documentation (function canonicalize-element)
  (:intro "The " (:lisp function canonicalize-element)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)
               (anchor docspec-element)))

(def-documentation (function format-docspec-element)
  (:intro "The " (:lisp function format-docspec-element)
          " function produces output for one docspec element.")
  (:callspec (style target-type element stream))
  (:params (style "Object specifying the style for the output.")
           (target-type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods.")
           (element "The documentation spec.")
           (stream "Output stream for the result."))
  (:properties (manual-section output-model)
               (anchor docspec-element)))

(def-documentation (function format-output-preitem)
  (:intro "The "
          (:lisp function format-output-preitem)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-postitem)
  (:intro "The "
          (:lisp function format-output-postitem)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-default-output-contents-sep)
  (:intro "The "
          (:lisp function format-default-output-contents-sep)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-trailer-docspec)
  (:intro "The "
          (:lisp function format-output-trailer-docspec)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-leader-docspec)
  (:intro "The "
          (:lisp function format-output-leader-docspec)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function output-contents-contents)
  (:intro "The accessor "
          (:lisp function output-contents-contents)
          " references the list holding the contents specified by a "
          (:lisp type output-contents)
          " object.")
  (:properties (manual-section output-model)
               (anchor output-contents)))

(def-documentation (variable *output-nesting-depth*)
  (:intro "Variable "
          (:lisp variable *output-nesting-depth*)
          " --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function format-doc-content-items)
  (:intro "The "
          (:lisp function format-doc-content-items)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-doc-content-item)
  (:intro "The "
          (:lisp function format-doc-content-item)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-contents-actual)
  (:intro "The "
          (:lisp function format-output-contents-actual)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-contents-sep)
  (:intro "The "
          (:lisp function format-output-contents-sep)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function process-standard-output-framework-form)
  (:intro "The "
          (:lisp function process-standard-output-framework-form)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)
               (anchor standard-output-framework)))

(def-documentation (function get-output-unit-title)
  (:intro "The "
          (:lisp function get-output-unit-title)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model) (anchor output-contents)))

(def-documentation (function get-output-unit-short-title)
  (:intro "The "
          (:lisp function get-output-unit-short-title)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model) (anchor output-contents)))

(def-documentation (function get-output-unit-author)
  (:intro "The "
          (:lisp function get-output-unit-author)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model) (anchor output-contents)))

(def-documentation (function get-output-unit-leader)
  (:intro "The "
          (:lisp function get-output-unit-leader)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model) (anchor output-contents)))

(def-documentation (function get-output-unit-trailer)
  (:intro "The "
          (:lisp function get-output-unit-trailer)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model) (anchor output-contents)))

(def-documentation (function format-output-leader-material)
  (:intro "The "
          (:lisp function format-output-leader-material)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-leader-sep)
  (:intro "The "
          (:lisp function format-output-leader-sep)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-leader-title)
  (:intro "The "
          (:lisp function format-output-leader-title)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (function format-output-trailer-material)
  (:intro "The "
          (:lisp function format-output-trailer-material)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-output-formatting)))

(def-documentation (type explicit-doc-element)
  (:intro "Class "
          (:lisp type explicit-doc-element)
          " --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function package-exports-p)
  (:intro "The "
          (:lisp function package-exports-p)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)))

(def-documentation (function locate-package-home)
  (:intro "The "
          (:lisp function locate-package-home)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)))


;;; -----------------------------------------------------------------
;;; styles

(def-documentation (type symbol-homing-style)
  (:intro "The " (:lisp type symbol-homing-style)
          " style allows documentation to find alternative home packages for symbols.  This mixin allows symbols which are developed as internal symbols in one package, but exported from another interface package.  Style classes inheriting from this mixin should define a method on "
          (:lisp function candidate-home-packages)
          ": if the given symbol is exported from any of the packages returned by this function, it will be documented as being in that package, disregarding any differing result of "
          (:lisp function symbol-package)
          ".")
  (:properties (manual-section styles)))

(def-documentation (function symbol-homes)
  (:intro "The " (:lisp function symbol-homes)
          " accessor references the list of candidate packages to which a symbol should first be attributed under a "
          (:lisp type symbol-homing-style) " instance")
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model) (anchor symbol-homing-style)))

(def-documentation (function use-internal-names)
  (:intro "The " (:lisp function use-internal-names)
          " accessor references the boolean field of a "
          (:lisp type symbol-homing-style) " instance which determines whether non-exported names should match for reporting symbol-package membership.")
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model) (anchor symbol-homing-style)))

#:symbol-homing-style #:use-internal-names

(def-documentation (type itemized-list-style)
  (:intro "The " (:lisp type itemized-list-style)
          " class is a mixin style, intended as one of a number of superclasses in a style definition.  This class arranges its contents in an itemized list, rather than the default arrangement of labeled sections.")
  (:properties (manual-section styles)))

(def-documentation (function format-itemized-list-start)
  (:intro "The " (:lisp function format-itemized-list-start)
          " function --- " (:fill-in))
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model-element-formatting)
               (anchor itemized-list-style)))

(def-documentation (function format-itemized-list-end)
  (:intro "The " (:lisp function format-itemized-list-end)
          " function --- " (:fill-in))
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model-element-formatting)
               (anchor itemized-list-style)))

(def-documentation (function format-itemized-list-item-start)
  (:intro "The " (:lisp function format-itemized-list-item-start)
          " function --- " (:fill-in))
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model-element-formatting)
               (anchor itemized-list-style)))

(def-documentation (function format-itemized-list-item-end)
  (:intro "The " (:lisp function format-itemized-list-item-end)
          " function --- " (:fill-in))
  (:callspec (style target-type spec))
  (:properties (manual-section standard-model-element-formatting)
               (anchor itemized-list-style)))

(def-documentation (function candidate-home-packages)
  (:intro "The " (:lisp function candidate-home-packages)
          " function returns a list of packages in which the given specification should first be checked as the exporter of its symbolic name.  Generally, methods of this function are specialized on the style, and ignore the other parameters.")
  (:callspec (style target-type spec))
  (:properties (manual-section styles)
               (anchor symbol-homing-style)))

(def-documentation (type docspec-par-latex-style)
  (:intro "The " (:lisp type docspec-par-latex-style)
          " style mixin inserts a paragraph break between consecutive documentation units.")
  (:properties (manual-section styles)))

(def-documentation (type docspec-fancy-header-latex-style)
  (:intro "The " (:lisp type docspec-fancy-header-latex-style)
          " style mixin adds fancy headers to the beginning of each docspec.  This class is a subclass of "
          (:lisp type docspec-par-latex-style)
          ".")
  (:properties (manual-section styles)))


;;; -----------------------------------------------------------------
;;; standard-model

(def-documentation (type standard-doc-target)
  (:intro "The " (:lisp type standard-doc-target)
          " class implements DefDoc's standard model for the information we store about each target type.")
  (:params (name "Symbol naming the target type.")
           (docstring-installer "Function called to install a docstring.  Functions stored in this slots should take two argument: the symbolic name of the item being documented, and its docspec object."))
  (:properties (manual-section targets)))

(def-documentation (type standard-doc-spec)
  (:intro "The " (:lisp type standard-doc-spec)
          " class implements the standard, default class used to represent documentation internally within defdoc.  Objects of this class accept calls to seven accessors "
          (:lisp function docspec-descriptive)
          ", "
          (:lisp function docspec-intro)
          ", "
          (:lisp function docspec-blurb)
          ", "
          (:lisp function docspec-details)
          ", "
          (:lisp function docspec-params)
          ", "
          (:lisp function docspec-callspecs)
          " and "
          (:lisp function docspec-deprecated)
          " as well as to the three accessors of the superclass "
          (:lisp type doc-spec)
          ".")
  (:properties (manual-section standard-model)))

(def-documentation (compiler-macro with-unpacked-standard-spec)
  (:intro "The " (:lisp compiler-macro with-unpacked-standard-spec)
          " macro provides deconstruction of "
          (:lisp compiler-macro standard-doc-spec)
          " objects.")
  (:properties (manual-section control)))

(def-documentation (function sequence-element-items)
  (:intro "The "
          (:lisp function sequence-element-items)
          " function --- "
          (:fill-in))
  (:callspec (standard-sequence))
  (:properties (manual-section standard-model-elements)
               (anchor standard-sequence)))

(def-documentation (function format-sequence-element-separator)
  (:intro "The "
          (:lisp function format-sequence-element-separator)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-element-formatting)
               (anchor standard-sequence)))

(def-documentation (type callspec-keyarg)
  (:intro "The " (:lisp type callspec-keyarg)
          " class --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type standard-doc-element)
  (:intro "The "
          (:lisp type standard-doc-element)
          " class is the common superclass of the standard element types, and is a subclass of "
          (:lisp type docspec-element) ".")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function callspec-item-to-lines)
  (:intro "The "
          (:lisp function callspec-item-to-lines)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-default-callspec-block-width)
  (:intro "The "
          (:lisp function get-default-callspec-block-width)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type standard-paragraph-list)
  (:intro "The " (:lisp type standard-paragraph-list)
          " class is a container for a sequence of elements, each taken to be a separate paragraph.  It implements the "
          (:lisp doc-element :paragraphs) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function paragraphlist-element-items)
  (:intro "The "
          (:lisp function paragraphlist-element-items)
          " accessor references the elements wrapped by a "
          (:lisp type standard-paragraph-list) " element.")
  (:callspec (standard-paragraph-list))
  (:properties (manual-section standard-model-elements)
               (anchor standard-paragraph-list)))

(def-documentation (type standard-sequence)
  (:intro "The " (:lisp type standard-sequence)
          " class is a container for a sequence of consecutive appended elements, and implements the "
          (:lisp doc-element :seq) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (type standard-plain-text)
  (:intro "The "
          (:lisp type standard-plain-text)
          " class wraps a string of plain text, and implements the "
          (:lisp doc-element :plain)
          " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function text-element-text)
  (:intro "The " (:lisp function text-element-text)
          " accessor returns the string of plain text wrapped by a "
          (:lisp type standard-plain-text) " instance.")
  (:callspec (standard-plain-text))
  (:properties (manual-section standard-model-elements)
               (anchor standard-plain-text)))

(def-documentation (type standard-lisp-name)
  (:intro "The " (:lisp type standard-lisp-name)
          " class represents a reference to a Lisp name, and implements the "
          (:lisp doc-element :lisp) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function lisp-name)
  (:intro "The " (:lisp function lisp-name)
          " function returns the symbol referenced by a "
          (:lisp type standard-lisp-name) " instance.")
  (:callspec (standard-lisp-name))
  (:properties (manual-section standard-model-elements)
               (anchor standard-lisp-name)))

(def-documentation (function lisp-name-kind)
  (:intro "The "
          (:lisp function lisp-name-kind)
          " function returns the target type referenced by a "
          (:lisp type standard-lisp-name) " instance.")
  (:callspec (standard-lisp-name))
  (:properties (manual-section standard-model-elements)
               (anchor standard-lisp-name)))

(def-documentation (type standard-emphasized)
  (:intro "The " (:lisp type standard-emphasized)
          " class wrap an element which should be emphasized in its presentation, and implements the "
          (:lisp doc-element :emph) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function emphasized-spec)
  (:intro "The "
          (:lisp function emphasized-spec)
          " function returns the wrapped spec of a "
          (:lisp type standard-emphasized) " instance.")
  (:callspec (standard-emphasized))
  (:properties (manual-section standard-model-elements)
               (anchor standard-emphasized)))

(def-documentation (type standard-fillin-place)
  (:intro "The " (:lisp type standard-fillin-place)
          " class represents a note to the author to complete documentation. It implements the "
          (:lisp doc-element :fill-in) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function sequence-element-items)
  (:intro "The " (:lisp function sequence-element-items)
          " function returns the list of items of a "
          (:lisp type standard-sequence) " instance.")
  (:callspec (standard-sequence))
  (:properties (manual-section standard-model-elements)
               (anchor standard-sequence)))

(def-documentation (type standard-output-framework)
  (:intro "The " (:lisp type standard-output-framework)
          " class represents " (:fill-in) ", and implements the "
          (:lisp doc-element :XXXXX) " element.")
  (:properties (manual-section standard-model)))

(def-documentation (function doc-label-name)
  (:intro "The " (:lisp function doc-label-name)
          " function --- "
          (:fill-in) " instance.")
  (:properties (manual-section control)))

(def-documentation (type standard-simple-list-environment)
  (:intro "Type " (:lisp type standard-simple-list-environment)
          " is the common superclass of elements that contain a sequence of subelements.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function list-element-env-tag)
  (:intro "The " (:lisp function list-element-env-tag)
          " accessor references the particular list type tag in a "
          (:lisp type standard-simple-list-environment) " object.")
  (:callspec (standard-simple-list-environment))
  (:properties (manual-section standard-model-elements)
               (anchor standard-simple-list-environment)))

(def-documentation (function list-element-options)
  (:intro "The " (:lisp function list-element-options)
          " accessor references the options given for a "
          (:lisp type standard-simple-list-environment) " object.")
  (:callspec (list-element))
  (:properties (manual-section standard-model-elements)
               (anchor standard-simple-list-environment)))

(def-documentation (function list-element-specs)
  (:intro "The " (:lisp function list-element-specs)
          " accessor references the contained element items in a "
          (:lisp type standard-simple-list-environment) " object.")
  (:callspec (list-element))
  (:properties (manual-section standard-model-elements)
               (anchor standard-simple-list-environment)))

(def-documentation (type standard-code)
  (:intro "The " (:lisp type standard-code)
          " class wraps a string of code which should be blocked off as-is, and implements the "
          (:lisp doc-element :code) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function code-element-string)
  (:intro "The " (:lisp function code-element-string)
          " accessor returns the string wrapped by a "
          (:lisp type standard-code) " object.")
  (:callspec (standard-code))
  (:properties (anchor standard-code)
               (manual-section standard-model-elements)))

(def-documentation (type standard-inline)
  (:intro "The " (:lisp type standard-inline)
          " class wraps a string of code which should be inlined into running text as-is, and implements the "
          (:lisp doc-element :inline) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function inline-element-string)
  (:intro "The " (:lisp function inline-element-string)
          " accessor returns the string wrapped by a "
          (:lisp type standard-inline) " instance.")
  (:callspec (code-element))
  (:properties (anchor standard-inline)
               (manual-section standard-model-elements)))

(def-documentation (function get-doc-tags)
  (:intro "The " (:lisp function get-doc-tags)
          " function --- " (:fill-in))
  (:callspec (name target-type))
  (:properties (manual-section control)))

(def-documentation (type standard-itemize)
  (:intro "The " (:lisp type standard-itemize)
          " class represents an itemized list, and implements the "
          (:lisp doc-element :itemize) " element.")
  (:properties (manual-section standard-model-elements)
               (anchor standard-simple-list-environment)))

(def-documentation (function callspec-suffix)
  (:intro "The "
          (:lisp function callspec-suffix)
          " function returns the suffix element of a "
          (:lisp type standard-callspec) " instance.")
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type standard-enumerate)
  (:intro "The " (:lisp type standard-enumerate)
          " class represents an enumerated list, and implements the "
          (:lisp doc-element :enumerate) " element.")
  (:properties (manual-section standard-model-elements)
               (anchor standard-simple-list-environment)))

(def-documentation (type standard-outputset-element)
  (:intro "The " (:lisp type standard-outputset-element)
          " class holds the name of a class corresponding to a piece of output to be included. It implements the "
          (:lisp doc-element :output-set) " element.")
  (:properties (manual-section standard-model-elements)))

(def-documentation (function output-elem-name)
  (:intro "The "
          (:lisp type output-elem-name)
          " function returns the output set name referenced by a "
          (:lisp type standard-outputset-element) " instance.")
  (:properties (manual-section standard-model-elements)
               (anchor standard-outputset-element)))

(def-documentation (function output-elem-style)
  (:intro "The " (:lisp type output-elem-style)
          " function returns the style, if any, referenced by a "
          (:lisp type standard-outputset-element) " instance.")
  (:properties (manual-section standard-model-elements)
               (anchor standard-outputset-element)))

(def-documentation (function output-elem-style-supp-p)
  (:intro "The " (:lisp type output-elem-style-supp-p)
          " function returns whether a style is named for a "
          (:lisp type standard-outputset-element) " instance.")
  (:properties (manual-section standard-model-elements)
               (anchor standard-outputset-element)))

(def-documentation (type callspec-sequence-of)
  (:intro "Class "
          (:lisp type callspec-sequence-of)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-callspec-sequence-of-repeated)
  (:intro "The "
          (:lisp function get-callspec-sequence-of-repeated)
          " function --- "
          (:fill-in))
  (:callspec (callspec-sequence-of))
  (:properties (manual-section standard-model-callspecs)
               (anchor callspec-sequence-of)))

(def-documentation (type callspec-optional)
  (:intro "Class "
          (:lisp type callspec-optional)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-callspec-optional-option)
  (:intro "The "
          (:lisp function get-callspec-optional-option)
          " function --- "
          (:fill-in))
  (:callspec (callspec-optional-item))
  (:properties (manual-section standard-model-callspecs)
               (anchor callspec-optional)))

(def-documentation (type callspec-bag-of)
  (:intro "Class "
          (:lisp type callspec-bag-of)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type callspec-one-of)
  (:intro "Class "
          (:lisp type callspec-one-of)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type callspec-items-holder)
  (:intro "Class "
          (:lisp type callspec-items-holde)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-callspec-holder-items)
  (:intro "The "
          (:lisp function get-callspec-holder-items)
          " function --- "
          (:fill-in))
  (:callspec (callspec-holder))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-output-framework)
  (:intro "The "
          (:lisp function get-output-framework)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model)))

(def-documentation (function output-framework-name)
  (:intro "The "
          (:lisp function output-framework-name)
          " function --- "
          (:fill-in))
  (:properties (manual-section output-model)
               (anchor output-framework)))

(def-documentation (function callspec-prefix)
  (:intro "The "
          (:lisp function callspec-prefix)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-doc-specs)
  (:intro "The "
          (:lisp function get-doc-specs)
          " function --- "
          (:fill-in))
  (:properties (manual-section control)))

(def-documentation (type callspec-keyheaded)
  (:intro "Class "
          (:lisp type callspec-keyheaded)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-callspec-keyheaded-key)
  (:intro "The "
          (:lisp function get-callspec-keyheaded-key)
          " function --- "
          (:fill-in))
  (:callspec (callspec-keyheaded))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function get-callspec-keyheaded-forms)
  (:intro "The "
          (:lisp function get-callspec-keyheaded-forms)
          " function --- "
          (:fill-in))
  (:callspec (callspec-keyheaded))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (type standard-doc-label)
  (:intro "The " (:lisp type standard-doc-label)
          " class --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (type standard-callspec)
  (:intro "Type "
          (:lisp type standard-callspec)
          " --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function callspec-to-lines)
  (:intro "The "
          (:lisp function callspec-to-lines)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function process-standard-labeldef-form)
  (:intro "The "
          (:lisp function process-standard-labeldef-form)
          " function --- "
          (:fill-in))
  (:properties (manual-section label-model)))

(def-documentation (function lower-case-target-name)
  (:intro "The "
          (:lisp function lower-case-target-name)
          " function --- "
          (:fill-in))
  (:properties (manual-section targets)))

(def-documentation (function capitalized-target-name)
  (:intro "The "
          (:lisp function capitalized-target-name)
          " function --- "
          (:fill-in))
  (:properties (manual-section targets)))

(def-documentation (function standard-callspec-key)
  (:intro "The "
          (:lisp function standard-callspec-key)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function get-callspec-keyarg-key)
  (:intro "The "
          (:lisp function get-callspec-keyarg-key)
          " function --- "
          (:fill-in))
  (:callspec (callspec-keyarg))
  (:properties (manual-section standard-model-callspecs)
               (anchor callspec-keyarg)))

(def-documentation (function standard-callspec-body-supp)
  (:intro "The "
          (:lisp function standard-callspec-body-supp)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-mandatory)
  (:intro "The "
          (:lisp function standard-callspec-mandatory)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-key-supp)
  (:intro "The "
          (:lisp function standard-callspec-key-supp)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function standard-callspec-optional)
  (:intro "The "
          (:lisp function standard-callspec-optional)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function get-callspec-keyarg-arg)
  (:intro "The "
          (:lisp function get-callspec-keyarg-arg)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function format-standard-docspec-callspec)
  (:intro "The "
          (:lisp function format-standard-docspec-callspec)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-element-formatting)))

(def-documentation (function format-standard-docspec-details-sep)
  (:intro "The "
          (:lisp function format-standard-docspec-details-sep)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function check-standard-docspec-details-sep)
  (:intro "The "
          (:lisp function check-standard-docspec-details-sep)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function standard-callspec-optional-supp)
  (:intro "The "
          (:lisp function standard-callspec-optional-supp)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (type macrolist-callspec)
  (:intro "The " (:lisp type macrolist-callspec)
          " class --- "
          (:fill-in))
  (:properties (manual-section standard-model-callspecs)))

(def-documentation (function format-standard-docspec-param-list-start)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list-start)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function format-standard-docspec-param-list-item-start)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list-item-start)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function format-standard-docspec-param-list-item-stop)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list-item-stop)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function format-standard-docspec-param-list-stop)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list-stop)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs-formatting)))

(def-documentation (function format-standard-docspec-param-list-item)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list-item)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs-formatting)))

(def-documentation (function standard-callspec-body)
  (:intro "The "
          (:lisp function standard-callspec-body)
          " function --- "
          (:fill-in))
  (:callspec (standard-callspec))
  (:properties (manual-section standard-model-callspecs)
               (anchor standard-callspec)))

(def-documentation (function format-standard-docspec-param-list)
  (:intro "The "
          (:lisp function format-standard-docspec-param-list)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))

(def-documentation (function format-standard-docspec-literal-text)
  (:intro "The "
          (:lisp function format-standard-docspec-literal-text)
          " function --- "
          (:fill-in))
  (:properties (manual-section standard-model-spec-formatting)))


;;; -----------------------------------------------------------------
;;; plaintext

(def-documentation (type standard-docstring-style)
  (:intro "The " (:lisp type standard-docstring-style)
          " class is a standard implementation of the default style for docstring generation.")
  (:properties (manual-section plaintext-docstrings)))

(def-documentation (function indent-with)
  (:intro "The " (:lisp function indent-with) " function --- " (:fill-in))
    (:callspec (lines length))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function adjoin-blocks)
   (:intro "The " (:lisp function adjoin-blocks) " function --- " (:fill-in))
    (:callspec (lines length))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function indent-by)
   (:intro "The " (:lisp function indent-by) " function --- " (:fill-in))
    (:callspec (lines length))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function bracket-with)
   (:intro "The " (:lisp function bracket-with) " function --- " (:fill-in))
    (:callspec (lines prefix suffix))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function width)
   (:intro "The " (:lisp function width) " function --- " (:fill-in))
    (:callspec (lines))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function whitespace-p)
   (:intro "The " (:lisp function whitespace-p) " function --- " (:fill-in))
    (:callspec (char))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function flow)
   (:intro "The " (:lisp function flow) " function --- " (:fill-in))
    (:callspec (formatter artifacts max))
  (:properties (manual-section plaintext-utils)))

(def-documentation (function output-lines)
  (:intro "The "
          (:lisp function output-lines)
          " function --- "
          (:fill-in))
  (:properties (manual-section plaintext-utils)))

(def-documentation (variable *docstring-style*)
  (:intro "The " (:lisp variable *docstring-style*)
          " variable specifies the style used when generating docstrings.")
  (:properties (manual-section plaintext-docstrings)))


;;; -----------------------------------------------------------------
;;; latex

(def-documentation (variable *latex-verbatim-width*)
  (:intro "The " (:lisp variable *latex-verbatim-width*)
          " variable specifies the maximum line length in verbatim mode")
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-spec-latex)
  (:intro "The " (:lisp function write-spec-latex)
          " function writes a single document spec to a "
          (:latex-name)
          " source file.")
  (:callspec (name usage &key
                   (style style)
                   (directory pathname)
                   (file filename-string)))
  (:params (name "Symbol naming the entity to be documented.")
           (usage "Symbol naming the target type of the entity, for example "
                  (:lisp symbol function)
                  " , "
                  (:lisp symbol type)
                  " , etc.")
           (style "Style class to be applied for this output; the default is "
                          (:lisp type latex-style)
                          ".")
           (directory "Directory where the output file should be written; the default is the value of "
                              (:lisp variable *defdoc-latex-default-directory*)
                              ".")
           (file "File to be created or overwritten with this content; by default the result of a call to "
                         (:lisp function get-latex-output-file-name)
                         " is used."))
  (:properties (manual-section latex)))

(def-documentation (variable *latex-full-package-item-header-macro*)
  (:intro "The " (:lisp variable *latex-full-package-item-header-macro*)
          " variable is a string with the "
          (:latex-name)
          " macro used as a section header in the "
          (:lisp type full-package)
          " style.")
  (:properties (manual-section latex-style-model)))

(def-documentation (variable *defdoc-latex-default-directory*)
  (:intro "The " (:lisp variable *defdoc-latex-default-directory*)
          " variable is a string naming the directry into which generated "
          (:latex-name)
          " output should be written.")
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-package-specs-latex)
  (:intro "The " (:lisp function write-package-specs-latex)
          " function generates "
          (:latex-name)
          " source files for all docspecs for entities named by symbols in a particular package.")
  (:callspec (package-spec &key (style name) (package-style name)
                           (include-doctypes ( (:seq doctype) ))
                           (directory pathname) (echo callback)))
  (:params (package-spec "Specifier for the package for which output is to be generated.")
           (style "Style class to be applied for this output; the default is "
                          (:lisp type latex-style)
                          ".")
           (package-style "When non-nil, indicated that a "
                          (:latex-name)
                          " file documenting the package itself should be written, and names the style class to be applied for that output file.  The default is "
                                  (:lisp symbol t)
                                  ", which indicates that the value of the "
                                  (:lisp keyword :style)
                                  " argument should be used here as well.")
           (doctype "This list of symbols naming spec type selects additional documentation to be written: when a type is given here, all docspecs are that type will be written "
                    (:emph "whether or not the specs are attached to entities named by symbols in the package targeted by this call")
                    ".")
           (directory "Directory where the output files should be written; the default is the value of "
                              (:lisp variable *defdoc-latex-default-directory*)
                              ".")
           (callback "The echo callback is invoked for each item written; the call is passed keyword arguments "
                             (:lisp keyword :name)
                             " and "
                             (:lisp keyword :type)
                             " for the symbols naming respectively the entity name and doctype.  The callback is useful for providing feedback on long processes to the user."))
  (:properties (manual-section latex)))

(def-documentation (function process-latex-document)
  (:intro "The "
          (:lisp function process-latex-document)
          " function is a utility function which runs "
          (:latex-name)
          " and associated tools on a particular file.")
  (:callspec (directory file-name-root &key (bibtex flag) (index flag)))
  (:params (directory "Directory holding the " (:latex-name) " source file.")
           (file-name-root (:latex "Root file name (that is, dropping the ``\\texttt{.tex}'' suffix) of the ")
                           (:latex-name) " source file.")
           (bibtex (:seq "If non-nil, indicates that " (:bibtex-name)
                         " should be used to generate a bibliography."))
           (index (:latex "If non-nil, indicates that \\texttt{makeindex} should be used to generate an index.")))
  (:details "Although " (:lisp function process-latex-document)
            " does not attempt to parse the output of the various tools, it will re-run "
            (:latex-name)
            " as it anticipates to be necessary to resolve cross-references, which may result in running the tool more times than is strictly necessary."
            "Invocation of external programs is not included in the Common Lisp specification, and varies by platform.  Thus, this function is implementated on a platform-by-platform basis.  At this writing, it is known to work on Allegro and Steel Bank Common Lisps only.")
  (:properties (manual-section latex)))

(def-documentation (function write-latex-output)
  (:intro "The " (:lisp function write-latex-output) " function --- " (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (function write-doctype-latex)
  (:intro "The " (:lisp function write-doctype-latex) " function generates "
          (:latex-name)
          " source files for all docspecs of a certain target type.")
  (:callspec (doctype &key (style style) (directory pathname) (echo callback)))
  (:params (doctype "Symbol naming the target type of the entities to be written, for example "
                    (:lisp symbol function) " or " (:lisp symbol type) ".")
           (style "Style class to be applied for this output; the default is "
                  (:lisp type latex-style) ".")
           (directory "Directory where the output files should be written; the default is the value of "
                      (:lisp variable *defdoc-latex-default-directory*) ".")
           (callback "The echo callback is invoked for each item written; the call is passed keyword arguments "
                     (:lisp keyword :name) " and " (:lisp keyword :type)
                     " for the symbols naming respectively the entity name and doctype.  The callback is useful for providing feedback on long processes to the user."))
  (:properties (manual-section latex)))

(def-documentation (function get-element-for-docspec-format)
  (:intro "The " (:lisp function get-element-for-docspec-format) " function --- "
          (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (type latex-style)
  (:intro "The " (:lisp type latex-style) " class is a base class for "
          (:latex-name) " document generation.")
  (:properties (manual-section styles)))

(def-documentation (type latex-name-element)
  (:intro "The " (:lisp type latex-name-element) " class --- " (:fill-in))
  (:properties (manual-section styles)))

(def-documentation (type bibtex-name-element)
  (:intro "The " (:lisp type bibtex-name-element) " class --- " (:fill-in))
  (:properties (manual-section styles)))

(def-documentation (type plaintext-style)
  (:intro "The " (:lisp type plaintext-style)
          " class is a base class for generating text documents.")
  (:properties (manual-section styles)))

(def-documentation (type full-package-latex-style-mixin)
  (:intro "Type " (:lisp type full-package-latex-style-mixin)
          " --- " (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (type standard-latex)
  (:intro "Type " (:lisp type standard-latex) " --- " (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (function latex-element-latex)
  (:intro "The " (:lisp function latex-element-latex) " function --- " (:fill-in))
  (:properties (manual-section latex-style-model)
               (anchor standard-latex)))

(def-documentation (function package-list-overall-header)
  (:intro "The " (:lisp function package-list-overall-header) " function --- "
          (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (function get-latex-output-file-name)
  (:intro "The " (:lisp function get-latex-output-file-name) " function --- "
          (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (type package-list-latex-mixin)
  (:intro "Type " (:lisp type package-list-latex-mixin) " --- " (:fill-in))
  (:properties (manual-section latex-style-model)))

(def-documentation (function index-lisp-name)
  (:intro "The " (:lisp function index-lisp-name) " function --- " (:fill-in))
  (:properties (manual-section latex-style-model)))


;;; -----------------------------------------------------------------
;;; HTML

(def-documentation (compiler-macro with-div-wrapper)
  (:intro "The " (:lisp compiler-macro with-div-wrapper) " macro " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (compiler-macro with-span-wrapper)
  (:intro "The " (:lisp compiler-macro with-span-wrapper) " macro " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (type html-style)
  (:intro "The " (:lisp type html-style)
          " class is a base class for HTML document generation.  Note that "
          (:latex-name)
          (:latex "'s \\texttt{makeindex} program can become confused if a directory has the same name as a ")
          (:latex-name)
          (:latex " source file: so a file \\texttt{mydoc.tex} and a generated directory of HTML files \\texttt{mydoc} should not actually be given the same name."))
  (:properties (manual-section styles)))

(def-documentation (function write-html-output-index-page)
  (:intro "The " (:lisp function write-html-output-index-page) " function "
          (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-header-block)
  (:intro "The " (:lisp function format-html-output-index-page-header-block)
          " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-headers)
  (:intro "The " (:lisp function format-html-output-index-page-headers)
          " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-html-output-index-page-body)
  (:intro "The " (:lisp function format-html-output-index-page-body)
          " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function traverse-and-write-output-pages)
  (:intro "The " (:lisp function traverse-and-write-output-pages) " function "
          (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function get-html-disambiguator)
  (:intro "The " (:lisp function get-html-disambiguator)
          " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-content-link)
  (:intro "The " (:lisp function format-content-link) " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function get-content-link-filename)
  (:intro "The " (:lisp function get-content-link-filename) " function "
          (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function format-content-anchor)
  (:intro "The " (:lisp function format-content-anchor) " function " (:fill-in))
  (:properties (manual-section html-style-model)))

(def-documentation (function html-free-paragraph-docspec-element)
  (:intro "The " (:lisp function html-free-paragraph-docspec-element)
          " function " (:fill-in))
  (:properties (manual-section html-style-model)))


;;; -----------------------------------------------------------------
;;; deprecated

(def-documentation (compiler-macro def-doc-tag)
  (:intro "The " (:lisp compiler-macro def-doc-tag) " macro is deprecated.")
  (:properties (manual-section deprecated)))

(def-documentation (function tag-sort)
  (:intro "The " (:lisp function tag-sort) " function --- " (:fill-in))
  (:properties (manual-section deprecated)))

(def-documentation (function format-tag)
  (:intro "The " (:lisp function format-tag) " function --- " (:fill-in))
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-group-header)
  (:intro "The " (:lisp function package-list-group-header) " function --- "
          (:fill-in))
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-entry)
  (:intro "The " (:lisp function package-list-entry) " function --- " (:fill-in))
  (:properties (manual-section deprecated)))

;;; -----------------------------------------------------------------
;;; Overall package documentation.

(def-documentation (package :defdoc)
    (:blurb "Container for high-level use of the structured documentation manager DefDoc.")
  (:descriptive "Defdoc"))

;;; -----------------------------------------------------------------
;;; Contracts
(def-documentation (type defdoc-interfaces:style-coverage)
  (:intro "Class "
          (:lisp type defdoc-interfaces:style-coverage)
          " --- "
          (:fill-in))
  (:properties (manual-section api-checks)))

(def-documentation (type plaintext-methods-coverage)
  (:intro "Class "
          (:lisp type plaintext-methods-coverage)
          " --- "
          (:fill-in))
  (:properties (manual-section api-checks)))

(def-documentation (type standard-elements-style-coverage)
  (:intro "Class "
          (:lisp type standard-elements-style-coverage)
          " --- "
          (:fill-in))
  (:properties (manual-section api-checks)))

;;; -----------------------------------------------------------------
;;; Issue warnings if anything is not documented.
(ensure-api-documentation :defdoc)
(ensure-api-documentation :defdoc-control-api)
