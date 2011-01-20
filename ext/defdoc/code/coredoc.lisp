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

(in-package :defdoc)

;; This eval-when is necessary on in this use of this method.  When
;; "normal" applications use DefDoc, they can place this defmethod in
;; e.g. the package.lisp or some other source file which will be
;; loaded before other files are compiled.
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod string-implicit-symbol-head ((p (eql (find-package :defdoc)))
                                          spec string)
    (declare (ignore spec string))
    :latex))

(def-property-label manual-section ())

;;; -----------------------------------------------------------------
;;; Standard documentation elements

(def-target-type doc-element ())

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

(def-target-type callspec-special ())

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

(def-documentation (compiler-macro def-property-label)
  (:intro "The \\texttt{def-property-label} macro declares a label for use in the \\texttt{:properties} of a documentation spec.")
  (:properties (manual-section docspecs))
  (:callspec (label-name () &body (:opt (:key-head :default-subsort name)))))


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
           (collection-form "One or more calls to the \\texttt{collect-} functions below.")
;;;           (property-values "A list of label-value pairs.  The labels should have been defined via \\texttt{def-property-label}.")
;;;           (grouping-label "Names a declared label.  The output will be divided into groups corresponding to the value stored against this label in the output set's contents.")
;;;           (groups "Names and orders the values corresponding to the groups induced by the \\texttt{grouping-label} form.  When this form is provided, any values not in the list are ignored in the output.")
;;;           (default-group "The default group to which specs which have no value stored for the \\texttt{grouping-label} property will be grouped.")
;;;           (target-type "Includes all documentation specs for the given target type in the output set.  This form may appear multiple times in the macro body.")
;;;           (all-symbols "Includes all symbols of the given package in the output set.  This form may appear multiple times in the macro body.")
;;;           (documented-symbols "Includes all documented symbols of the given package in the output set.  This form may appear multiple times in the macro body.")
;;;           (exported-symbols "Includes all exported symbols of the given package in the output set.  This form may appear multiple times in the macro body.")
;;;           (with-output "Names output sets which should be taken as subcomponents of this output.")
           )
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro def-label-config)
  (:intro "The \\texttt{def-label-config} macro specifies information associated with particular uses, especially particular outputs, of a property label.")
  (:callspec ((&key (label label-name)
                    (style style)
                    (output-framework output-name)
                    (package package))
              &body (:seq (value &key (title group-title)
                                 (order symbol-list)))))
  (:params (label-name "\\fbox{FILL IN}")
           (style "\\fbox{FILL IN}")
           (output-name "\\fbox{FILL IN}")
           (package "\\fbox{FILL IN}")
           (value "\\fbox{FILL IN}")
           (group-title "\\fbox{FILL IN}")
           (symbol-list "\\fbox{FILL IN}"))
  (:properties (manual-section outspec)))

(def-documentation (compiler-macro collect-groups-by-label)
  (:intro "Macro \\texttt{collect-groups-by-label} --- \\fbox{FILL IN}")
  (:callspec ((label-nam &key (package package) (groups groups))
              &body (:seq collection-form)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-target-type)
  (:intro "Function \\texttt{collect-target-type} --- \\fbox{FILL IN}")
  (:callspec (target-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-exported-symbols)
  (:intro "Function \\texttt{collect-exported-symbols} --- \\fbox{FILL IN}")
  (:callspec (package-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-documented-symbols)
  (:intro "Function \\texttt{collect-documented-symbols} --- \\fbox{FILL IN}")
  (:callspec (package-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-all-symbols)
  (:intro "Function \\texttt{collect-all-symbols} --- \\fbox{FILL IN}")
  (:callspec (package-name (:seq filter)))
  (:properties (manual-section outspec)))

(def-documentation (function collect-output)
  (:intro "Function \\texttt{collect-output} --- \\fbox{FILL IN}")
  (:properties (manual-section outspec)))

(def-documentation (function collect-named-output)
  (:intro "Function \\texttt{collect-named-output} --- \\fbox{FILL IN}")
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

(def-documentation (compiler-macro def-target-type)
  (:intro "The \\texttt{def-target-type} macro defines a new documentation target type.")
  (:callspec (name (&key (class class-name))
                   &body (:key-head docstring-installer (target-name target-spec) (:seq form))))
  (:params (name "Symbolic name of the new target type.")
           (class-name "Class to be used as a record for the stored target type's information.  The default is \\texttt{standard-doc-target}; if another class is used it must support the \\texttt{:name} initarg and \\texttt{docstring-installer} accessor.")
           (docstring-installer "Function which installs a standard Lisp document string for targets of this type."))
  (:properties (manual-section docspecs)))


;;; -----------------------------------------------------------------
;;; model

(def-documentation (variable *spec-class*)
  (:intro "The \\texttt{*spec-class*} variable specifies the name of the class used by default as the representation for a documentation specification.")
  (:properties (manual-section model)))

(def-documentation (type doc-spec)
  (:intro "The \\texttt{doc-spec} class is the common superclass of documentation spec classes.  Accepts calls to the accessors \\texttt{docspec-self}, \\texttt{docspec-target-type} and \\texttt{docspec-tags}.")
  (:properties (manual-section model)))

(def-documentation (function docspec-self)
    (:intro "The \\texttt{docspec-self} accessor on documentation objects retrieves the symbolic name of the target of the documentation.")
  (:properties (manual-section model)))

(def-documentation (function docspec-target-type)
    (:intro "The \\texttt{docspec-target-type} accessor on documentation objects retrieves the symbolic name of the target type of the documentation.")
  (:properties (manual-section model)))

(def-documentation (function docspec-tags)
    (:intro "The \\texttt{docspec-tags} accessor on documentation objects retrieves the tags associated with a particular piece of documentation.")
  (:properties (manual-section model)))

(def-documentation (function docspec-descriptive)
    (:intro "The \\texttt{docspec-descriptive} accessor on standard documentation objects retrieves a one-word descriptive string for the target.  Used in FILL IN")
  (:properties (manual-section model)))

(def-documentation (function docspec-intro)
    (:intro "The \\texttt{docspec-intro} accessor on standard documentation objects retrieves the introduction element.")
  (:properties (manual-section model)))

(def-documentation (function docspec-blurb)
    (:intro "The \\texttt{docspec-blurb} accessor on standard documentation objects retrieves the short description element.")
  (:properties (manual-section model)))

(def-documentation (function docspec-details)
    (:intro "The \\texttt{docspec-details} accessor on standard documentation objects retrieves the documentation's main body.")
  (:properties (manual-section model)))

(def-documentation (function docspec-params)
    (:intro "The \\texttt{docspec-params} accessor on standard documentation objects retrieves documentation of the object's parameters.")
  (:properties (manual-section model)))

(def-documentation (function docspec-callspecs)
    (:intro "The \\texttt{docspec-callspecs} accessor on standard documentation objects retrieves the callspec element.")
  (:properties (manual-section model)))

(def-documentation (function docspec-deprecated)
    (:intro "The \\texttt{docspec-deprecated} accessor on standard documentation objects retrieves a deprecation flag.")
  (:properties (manual-section model)))

(def-documentation (function get-label-symbol-value-translation)
  (:intro "Function \\texttt{get-label-symbol-value-translation} --- \\fbox{FILL IN}")
  (:properties (manual-section model)))

(def-documentation (function get-target-type-docspecs)
  (:intro "Function \\texttt{get-target-type-docspecs} --- \\fbox{FILL IN}")
  (:properties (manual-section model)))

(def-documentation (type doc-label)
  (:intro "Type \\texttt{doc-label} --- \\fbox{FILL IN}")
  (:properties (manual-section model)))

;;;(def-documentation (function get-compiled-output-framework)
;;;  (:intro "Function \\texttt{get-compiled-output-framework} --- \\fbox{FILL IN}")
;;;  (:properties (manual-section model)))

;;;(def-documentation (function get-output-framework-class)
;;;  (:intro "Function \\texttt{get-output-framework-class} --- \\fbox{FILL IN}")
;;;  (:properties (manual-section model)))


;;; -----------------------------------------------------------------
;;; label-model

(def-documentation (function get-label-class)
  (:intro "Function \\texttt{get-label-class} --- \\fbox{FILL IN}")
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


;;; -----------------------------------------------------------------
;;; elements

(def-documentation (compiler-macro def-element)
  (:intro "Compiler-macro \\texttt{def-element} --- \\fbox{FILL IN}")
  (:properties (manual-section elements))
)

(def-documentation (function compile-string-element)
  (:intro "Function \\texttt{compile-string-element} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function text-element-text)
  (:intro "Function \\texttt{text-element-text} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (variable *default-element-class*)
  (:intro "Variable \\texttt{*default-element-class*} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function list-element-env-tag)
  (:intro "Function \\texttt{list-element-env-tag} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function list-element-options)
  (:intro "Function \\texttt{list-element-options} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function code-element-string)
  (:intro "Function \\texttt{code-element-string} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function list-element-specs)
  (:intro "Function \\texttt{list-element-specs} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function sequence-element-items)
  (:intro "Function \\texttt{sequence-element-items} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

(def-documentation (function paragraphlist-element-items)
  (:intro "Function \\texttt{paragraphlist-element-items} --- \\fbox{FILL IN}")
  (:properties (manual-section elements)))

;;; -----------------------------------------------------------------
;;; output-model

(def-documentation (function format-docspec)
  (:intro "The \\texttt{format-docspec} function produces output from a documentation specification.")
  (:callspec (stream style spec target-type))
  (:params (stream "Output stream for the result.")
           (style "Object specifying the style for the output.")
           (spec "The documentation spec.")
           (type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods."))
    (:properties (manual-section output-model)))

(def-documentation (function format-docspec-element)
  (:intro "The \\texttt{format-docspec-element} function produces output for one docspec element.")
  (:callspec (style target-type element stream))
  (:params (style "Object specifying the style for the output.")
           (target-type "The target-type of the spec.  This value is redundant, since it can be retrieved from the spec, but is passed for dispatch by applications' methods.")
           (element "The documentation spec.")
           (stream "Output stream for the result."))
  (:properties (manual-section output-model)))

(def-documentation (function format-output-preitem)
  (:intro "Function \\texttt{format-output-preitem} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function format-output-postitem)
  (:intro "Function \\texttt{format-output-postitem} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))

(def-documentation (function process-standard-output-framework-form)
  (:intro "Function \\texttt{process-standard-output-framework-form} --- \\fbox{FILL IN}")
  (:properties (manual-section output-model)))


;;; -----------------------------------------------------------------
;;; standard-model

(def-documentation (type standard-doc-target)
    (:intro "The \\texttt{standard-doc-target} class implements DefDoc's standard model for the information we store about each target type.")
  (:params (name "Symbol naming the target type.")
           (docstring-installer "Function called to install a docstring.  Functions stored in this slots should take two argument: the symbolic name of the item being documented, and its docspec object."))
  (:properties (manual-section standard-model)))

(def-documentation (type standard-doc-spec)
  (:intro "The \\texttt{standard-doc-spec} class implements the standard, default class used to represent documentation internally within defdoc.  Objects of this class accept calls to seven accessors \\texttt{docspec-descriptive}, \\texttt{docspec-intro}, \\texttt{docspec-blurb}, \\texttt{docspec-details}, \\texttt{docspec-params}, \\texttt{docspec-callspecs} and \\texttt{docspec-deprecated} as well as to the three accessors of the superclass doc-spec.")
  (:properties (manual-section standard-model)))

(def-documentation (compiler-macro with-unpacked-standard-spec)
    (:intro "The \\texttt{with-unpacked-standard-spec} macro provides deconstruction of \\texttt{standard-doc-spec} objects.")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-sequence)
  (:intro "Type \\texttt{standard-sequence} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-keyarg)
  (:intro "Function \\texttt{callspec-keyarg} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function standard-doc-element)
  (:intro "Function \\texttt{standard-doc-element} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-item-to-lines)
  (:intro "Function \\texttt{callspec-item-to-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function standard-paragraph-list)
  (:intro "Function \\texttt{standard-paragraph-list} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type output-framework)
  (:intro "Type \\texttt{output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-output-framework)
  (:intro "Type \\texttt{standard-output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function doc-label-name)
  (:intro "Function \\texttt{doc-label-name} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-simple-list-environment)
  (:intro "Type \\texttt{standard-simple-list-environment} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-code)
  (:intro "Type \\texttt{standard-code} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-doc-tags)
  (:intro "Function \\texttt{get-doc-tags} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-itemize)
  (:intro "Type \\texttt{standard-itemize} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-suffix)
  (:intro "Function \\texttt{callspec-suffix} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-enumerate)
  (:intro "Type \\texttt{standard-enumerate} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-sequence-of)
  (:intro "Function \\texttt{callspec-sequence-of} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-output-framework)
  (:intro "Function \\texttt{get-output-framework} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function output-framework-name)
  (:intro "Function \\texttt{output-framework-name} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-prefix)
  (:intro "Function \\texttt{callspec-prefix} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function get-doc-specs)
  (:intro "Function \\texttt{get-doc-specs} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-keyheaded)
  (:intro "Function \\texttt{callspec-keyheaded} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function standard-doc-label)
  (:intro "Function \\texttt{standard-doc-label} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (type standard-callspec)
  (:intro "Type \\texttt{standard-callspec} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-to-lines)
  (:intro "Function \\texttt{callspec-to-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function callspec-optional)
  (:intro "Function \\texttt{callspec-optional} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))

(def-documentation (function process-standard-labeldef-form)
  (:intro "Function \\texttt{process-standard-labeldef-form} --- \\fbox{FILL IN}")
  (:properties (manual-section standard-model)))


;;; -----------------------------------------------------------------
;;; plaintext

(def-documentation (type standard-docstring-style)
    (:intro "The \\texttt{standard-docstring-style} class is a standard implementation of the default style for docstring generation.")
  (:properties (manual-section plaintext)))

(def-documentation (type standard-plain-text)
  (:intro "Type \\texttt{standard-plain-text} --- \\fbox{FILL IN}")
  (:properties (manual-section plaintext)))

(def-documentation (function indent-with)
    (:callspec (lines length))
  (:properties (manual-section plaintext)))

(def-documentation (function adjoin-blocks)
    (:callspec (lines length))
  (:properties (manual-section plaintext)))

(def-documentation (function indent-by)
    (:callspec (lines length))
  (:properties (manual-section plaintext)))

(def-documentation (function bracket-with)
    (:callspec (lines prefix suffix))
  (:properties (manual-section plaintext)))

(def-documentation (function width)
    (:callspec (lines))
  (:properties (manual-section plaintext)))

(def-documentation (function flow)
    (:callspec (formatter artifacts max))
  (:properties (manual-section plaintext)))

(def-documentation (function output-lines)
  (:intro "Function \\texttt{output-lines} --- \\fbox{FILL IN}")
  (:properties (manual-section plaintext)))

(def-documentation (variable *docstring-style*)
  (:intro "The \\texttt{*docstring-style*} variable specifies the style used when generating docstrings.")
  (:properties (manual-section plaintext)))


;;; -----------------------------------------------------------------
;;; latex

(def-documentation (variable *latex-verbatim-width*)
    (:intro "The \\texttt{*latex-verbatim-width*} variable specifies the maximum line length in verbatim mode")
  (:properties (manual-section latex)))

(def-documentation (function write-spec-latex)
  (:intro "Function \\texttt{write-spec-latex} --- \\fbox{FILL IN}")
  (:callspec (name usage &key
                   (style style)
                   (directory pathname)
                   (file filename-string)))
  (:properties (manual-section latex)))

(def-documentation (variable *latex-full-package-item-header-macro*)
    (:intro "The \\texttt{*latex-full-package-item-header-macro*} variable is a string with the \\LaTeX\\ macro used as a section header in the \\texttt{full-package} style.")
  (:properties (manual-section latex)))

(def-documentation (variable *defdoc-latex-default-directory*)
  (:intro "The \\texttt{*defdoc-latex-default-directory*} variable is a string naming the directry into which generated \\LaTeX\\ output should be written.")
  (:properties (manual-section latex)))

(def-documentation (function write-package-specs-latex)
  (:intro "Function \\texttt{write-package-specs-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function process-latex-document)
  (:intro "Function \\texttt{process-latex-document} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function write-latex-output)
  (:intro "Function \\texttt{write-latex-output} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function write-doctype-latex)
  (:intro "Function \\texttt{write-doctype-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function latex-style-adjust-spec-element)
  (:intro "Function \\texttt{latex-style-adjust-spec-element} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (type latex-style)
  (:intro "Type \\texttt{latex-style} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (type full-package-latex-style-mixin)
  (:intro "Type \\texttt{full-package-latex-style-mixin} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (type standard-latex)
  (:intro "Type \\texttt{standard-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function latex-element-latex)
  (:intro "Function \\texttt{latex-element-latex} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function package-list-overall-header)
  (:intro "Function \\texttt{package-list-overall-header} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (function get-latex-output-file-name)
  (:intro "Function \\texttt{get-latex-output-file-name} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))

(def-documentation (type package-list-latex-mixin)
  (:intro "Type \\texttt{package-list-latex-mixin} --- \\fbox{FILL IN}")
  (:properties (manual-section latex)))


;;; -----------------------------------------------------------------
;;; deprecated

(def-documentation (compiler-macro def-doc-tag)
  (:intro "The \\texttt{def-doc-tag} macro is deprecated.")
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-group-header)
  (:intro "Function \\texttt{package-list-group-header} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

(def-documentation (function package-list-entry)
  (:intro "Function \\texttt{package-list-entry} --- \\fbox{FILL IN}")
  (:properties (manual-section deprecated)))

;;; -----------------------------------------------------------------

(def-documentation (package :defdoc)
    (:blurb "Container for high-level use of the structured documentation manager DefDoc.")
  (:descriptive "Defdoc"))
