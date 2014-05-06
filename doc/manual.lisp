;;; File manual.lisp
;;;
;;; This file is part of the NST unit/regression testing system.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lisp Lesser General Public License,
;;; which consists of the preamble published by Franz Incorporated,
;;; plus the LGPL published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lisp Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the Preamble to the Gnu Lesser
;;; General Public License and the GNU Lesser General Public License
;;; along with NST.  If not, see respectively
;;; <http://opensource.franz.com/preamble.html> and
;;; <http://www.gnu.org/licenses/>.
(in-package :nst-doc)

;;; --------------------------------------------------
;;; Style for the manual.

(def-latex-style-class manual-style-mixin () ()
                       (:usepackage '(|array| (|hyperref| |pdftex|))
                        :primary-tocdepth 1
                        :parskip "0.6em"
                        :contextualized-parskip ((:toc "-0.7em"))
                        :parindent "0em"))

(defmethod format-latex-pre-output-leader-material ((style manual-style-mixin)
                                                    stream output
                                                    &key &allow-other-keys)
  (declare (ignore stream output)))

(defmethod format-doc :around (stream (style manual-style-mixin) spec
                                      &key &allow-other-keys)
  (declare (ignore stream spec))
  (let ((*primary-tocdepth* 1)
        (*aftermatter-tocdepth* 1))
    (declare (special *primary-tocdepth* *aftermatter-tocdepth*))
    (call-next-method)))
(defmethod defdoc-control-api:get-latex-output-file-name ((style manual-style-mixin)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc-control-api:get-element-for-docspec-format ((style manual-style-mixin)
                                                   target-type spec
                                                   (element (eql :intro))
                                                   datum)
  (declare (ignore datum))
  (with-accessors ((self defdoc-control-api:docspec-self)) spec
    (cond
      ((eq target-type 'nst::criterion)
       (call-next-method))
      (t
       (make-instance 'defdoc-control-api:standard-sequence
         :elements (list (make-instance 'defdoc-control-api:standard-latex
                           :latex (format nil "\\label{~a:primary}" self))
                         (call-next-method)))))))
(defmethod defdoc-control-api:get-element-for-docspec-format ((style manual-style-mixin)
                                                   target-type spec
                                                   (element (eql :blurb))
                                                   datum)
  (declare (ignore datum target-type))
  (with-accessors ((self defdoc-control-api:docspec-self)) spec
    (make-instance 'defdoc-control-api:standard-sequence
      :elements (list (make-instance 'defdoc-control-api:standard-latex
                        :latex (format nil "\\label{~a:primary}" self))
                      (call-next-method)))))

(defmethod defdoc-control-api:format-docspec-element
    ((style manual-style-mixin) (target-type (eql 'nst::criterion))
     (spec defdoc-control-api:standard-doc-spec) stream &key &allow-other-keys)
  (with-accessors ((self defdoc-control-api:docspec-self)) spec
    (format stream "\\subsubsection{The \\texttt{~s} criterion}" self)
    (call-next-method)))

(defmethod defdoc-latex::format-latex-standalone-header
    :after ((style manual-style-mixin) stream (out nst-output-toplevel)
            &optional contents index)
  (declare (ignore contents index))
  (with-accessors ((contribs contribs)) out
    (when contribs
      (format stream "\\paragraph{Contributors.}")
      (format-docspec-element style out contribs stream))))

(defclass manual-primary-style (manual-style-mixin
                                latex-style docspec-par-latex-style) ())
(defclass manual-itemize-style (manual-style-mixin
                                latex-style docspec-itemize-latex-style) ())

;;; --------------------------------------------------
;;; Style for the manual's package list.

(defclass nst-package-list-latex-style (defdoc-control-api:package-list-latex-mixin
                                        defdoc:latex-style) ())
(defmethod defdoc-control-api:package-list-entry ((style nst-package-list-latex-style)
                                      spec group entry stream)
     (declare (ignore spec group))
     (let ((self (defdoc-control-api:docspec-self entry)))
       (format stream
           "\\texttt{~a} --- \\S\\ref{~:*~a:primary}, p.\\,\\pageref{~:*~a:primary}.~%~%"
         self)))


(defdoc:def-output-class (the-manual :class nst-output-toplevel
                                     :title "NST 4.0 User Manual"
                                     :leader (:latex "This document is the manual and users' guide to the 4.0.$x$
series of the NST test framework, last updated for 4.0.0.  NST is a
unit test system for Common Lisp which provides support for test
fixture data, stateful setup and cleanup of tests, grouping of tests,
and (we think!) a useful runtime interface.  Suggestions and comments
are welcome.  The files in the NST distribution's \\texttt{self-test}
directory, especially \\texttt{self-test/core/builtin-checks.lisp},
holds the NST tests for NST and contain many examples (some of which
we have adapted for this manual).  Known bugs and infelicities,
platform-specific release notes, and other technical materials are
available via the link on NST's CLiki page, \\textsl{cliki.net/NST}\\enspace."))
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

    ;; (defdoc:collect-exported-symbols :nst)
    (defdoc:collect-groups-by-label
        (nst::nst-manual :groups '((nst::fixtures :title "Fixtures"
                                                  :order (def-fixtures with-fixtures))
                                   (nst::groups :title "Test groups"))
                         :package :nst)
        (defdoc:collect-exported-symbols :nst))

  (collect-output (:title "Testing values" :short-title "Tests") ()
    (defdoc:collect-groups-by-label (nst::nst-manual :groups '(nst::tests))
      (defdoc:collect-exported-symbols :nst))
    (defdoc:collect-groups-by-label
        (nst::nst-manual :groups '((nst::basic-criteria :title "Basic criteria"
                                                        :order (:true :eq :symbol :eql :equal :equalp
                                                                      :forms-eq :forms-eql :forms-equal
                                                                      :predicate :err :perf))
                                   (nst::compound-criteria
                                    :title "Compound criteria"
                                    :order (:not :all :any :apply :check-err
                                            :progn :proj))
                                   (nst::multiple-values-criteria
                                    :title "Criteria for multiple values"
                                    :order ())
                                   (nst::list-criteria
                                    :title "Criteria for lists"
                                    :order ())
                                   (nst::vector-criteria
                                    :title "Criteria for vectors"
                                    :order ())
                                   (nst::class-criteria
                                    :title "Criteria for classes"
                                    :order ())
                                   (nst::misc-criteria :title
                                    "Programmatic and debugging criteria"
                                    :order ()))
                         :package :nst)
      (defdoc:collect-target-type 'nst::criterion))
    )
  (collect-output (:title "Testing processes"
                   :short-title "Process tests"
                   :leader (:seq "
The test criteria of the previous section all examined the result
of evaluating the forms under test.  This section presents NST's
criteria for validating the " (:emph "process") " of a computation,
specifying assertions which should hold at the initial, intermediate
and final points of the process."))
      ()
    (defdoc:collect-groups-by-label (nst::nst-manual :groups '(nst::process))
      (defdoc:collect-target-type 'nst::criterion))
    (defdoc:collect-groups-by-label (nst::nst-manual :groups '(nst::process))
      (defdoc:collect-exported-symbols :nst))
    (collect-output (:title "Asserting properties") ()
      (defdoc:collect-groups-by-label
          (nst::nst-manual :groups '((nst::process-predicate
                                      :order '(assert-criterion))))
        (defdoc:collect-exported-symbols :nst)))
    (collect-output (:title "Defining new assertion functions"
                     :short-title "New asserters") ()
      (defdoc:collect-groups-by-label (nst::nst-manual
                                       :groups '(nst::process-pred-maker))
        (defdoc:collect-exported-symbols :nst)))
    (collect-output (:title "A simpler process checker") ()
      (defdoc:collect-groups-by-label (nst::nst-manual
                                       :groups '(nst::process-dep))
        (defdoc:collect-target-type 'nst::criterion))))
  (collect-output (:title "Testing invariants against sampled data"
                          :short-title "Invariants"
                          :leader (:seq "The " (:lisp criterion :sample) (:latex " criterion provides random
generation of data for validating program properties.  Our approach is
based on Claessen and Hughes's Quickcheck\\footnote{Koen Claessen and
  John Hughes, ``QuickCheck: a lightweight tool for random testing of
  Haskell programs,'' from \\emph{Proceedings of the International
    Conference on Functional Programming}, 2000.  QuickCheck papers,
  code and other resources are available at
  \\textsl{www.cs.chalmers.se/\~{}rjmh/QuickCheck}~.}.
\\par
This style of testing is somewhat more complicated than specific tests
on single, bespoke forms.  There are two distinct efforts, which we
address in the next two sections: describing how the sample data is to
be generated, and specifying the test itself."))) ()
    (collect-output (:title "Generating sample data") ()
      (collect-doc () (:seq "Data generation is centered around the generic function " (:lisp function arbitrary) ". "))
      (collect-symbols :nst (#:arbitrary))
      (collect-doc ()
                   (:paragraphs
                    (:seq "NST provides method of "
                          (:emph "arbitrary")
                          " for many standard Lisp types, listed in Table"
                          (:latex "~\\ref{table:built-in-arbitrary-types}.  ")
                          "Types in the first column --- the standard numeric types plus the common supertype "
                          (:lisp symbol t)
                          " are not associated with additional keyword arguments."
                          (:code "  (nst:arbitrary t)
  (nst:arbitrary 'complex)
  (nst:arbitrary 'integer)
  (nst:arbitrary 'ratio)
  (nst:arbitrary 'single-float)")
                          "Keyword arguments for other NST-provided type specifiers are as follows:"
                          (:latex "\\begin{table}
\\begin{center}
  \\texttt{
    \\begin{tabular}{lll|ll||l}
      \\multicolumn{5}{c}{\\textrm{\\textbf{Standard Lisp types}}}
          & \\multicolumn{1}{c}{\\textrm{\\textbf{Other types}}}
      \\\\ \\hline
         number  & character & symbol & cons   & hash-table & scalar
      \\\\ real & string    &        & list   & &
      \\\\ rational &       &        & vector & &
      \\\\ integer  &       &        & array  & &
      \\\\ float &&&&t&
      \\\\ fixnum &&&& &
      \\\\ bignum &&&& &
      \\\\ ratio &&&& &
      \\\\ \\multicolumn{2}{l}{short-float\\footnotemark} &&& &
      \\\\ \\multicolumn{2}{l}{single-float} &&& &
      \\\\ \\multicolumn{2}{l}{double-float\\addtocounter{footnote}{-1}\\footnotemark} &&& &
      \\\\ \\multicolumn{2}{l}{long-float} &&& &
      \\\\ complex &&&& &
      \\\\ \\cline{1-3} \\multicolumn{3}{c}{\\textrm{Considered \\texttt{scalar}}}
    \\end{tabular}}
\\end{center}
\\caption{NST provides methods of generic function \\texttt{arbitrary} generating values of the types in this table.}
\\label{table:built-in-arbitrary-types}
\\end{table}
\\footnotetext{Not available on Allegro Lisp.}"))

                    (:itemize ()
                      (:seq "Types " (:lisp type character)
                            " and " (:lisp type string) ":"
                            (:itemize ()
                              (:seq "Argument " (:lisp param noncontrol)
                                    ".  Excludes the control characters associated with ASCII code 0 through 31.")
                              (:seq "Argument "
                                    (:lisp param range)
                                    ".  Allows the range of characters to be restricted to a particular subset:"
                                    (:latex "\\begin{center}
      \\begin{tabular}{>{\\ttfamily:}c|l}
        \\multicolumn{1}{c|}{Value} & \\multicolumn{1}{c}{Meaning}
        \\\\ \\hline standard & Codes up to 96
        \\\\ ascii & Codes through 127
        \\\\ ascii-ext & Codes through 255
      \\end{tabular}
    \\end{center}")
                                    "Omitted or with any other value, characters with any code up to "
                                    (:lisp param char-code-limit)
                                    " can result.  Examples:"
                                    (:code "  (nst:arbitrary 'character)
  (nst:arbitrary '(character :noncontrol t
                             :range :standard))"))))

                      (:seq "Type " (:lisp type symbol) "."
                            (:itemize ()
                              (:seq "Argument " (:lisp param existing)
                                    ". If non-nil, requires that the result be a previously-interned symbol.")
                              (:seq "Argument " (:lisp param exported)
                                    ". Requires that the result be not only a previously-interned symbol, but also one exported by its package.  Ignored if "
                                    (:lisp keyword :existing)
                                    " is explicitly set to nil.")
                              (:seq "Argument " (:lisp param package)
                                    ". Specifies the package from which the symbol will be generated.  If omitted, a package is selected at random from the existing ones.")
                              (:seq "Argument " (:lisp param nonnull)
                                    ". If non-nil, allows " (:lisp function arbitrary)
                                    " to ignore other restriction to guarantee returning a non-nil symbol.  When null, "
                                    (:lisp function arbitrary) " may return nil.")
                              (:seq "Argument " (:lisp param gensym)
                                    ". If non-nil, and if "
                                    (:lisp function arbitrary)
                                    " is explicitly set to nil, returns a new uninterned symbol.")))

                      (:seq "Type " (:lisp type cons) "."
                            (:itemize ()
                              (:seq "Arguments " (:lisp param car)
                                    " and " (:lisp param cdr)
                                    " should be additional type specifications, used direct the generation of respectively the left and right elements of the result.  Each defaults to "
                                    (:lisp symbol t) ".")))

                      (:seq "Type " (:lisp type list) " and " (:lisp type vector) "."
                            (:itemize ()
                              (:seq "Argument " (:lisp param length)
                                    " specifies the length of the structure. If omitted, will be randomly generated.")
                              (:seq "Argument " (:lisp param elem)
                                    " directs the generation of the container's elements.  For both, the default element type is "
                                    (:lisp symbol t) ".")))

                      (:seq "Type " (:lisp type array) "."
                            (:itemize ()
                              (:seq "Argument " (:lisp param elem) ". As for "
                                    (:lisp type list) " and " (:lisp type vector) ".")
                              (:seq "Argument " (:lisp param dimens)
                                    ".  Should be a list of nonnegative
    integers specifying the length of each dimension of the array.  If
    omitted, will be randomly generated.")
                              (:seq "Argument " (:lisp param rank)
                                    ". Specifies the number of dimensions. If omitted but "
                                    (:lisp param dimens)
                                    " is given, will be set to the length of "
                                    (:lisp param dimens)
                                    ".  If both "
                                    (:lisp param rank)
                                    " and "
                                    (:lisp param dimens)
                                    " are omitted, then both are randomly generated.")))

                      (:seq "Type " (:lisp type hash-table) "."
                            (:itemize ()
                              (:seq "Argument " (:lisp param size)
                                    ". Specifies the number of entries in
    the table.  If omitted, will be randomly generated.")
                              (:seq "Argument " (:lisp param test)
                                    ". Specifies the hash table's test function.  If omitted, will be randomly selected from "
                                    (:lisp function eq) ", "
                                    (:lisp function eql) ", "
                                    (:lisp function equal) " and "
                                    (:lisp function equalp) ".")
                              (:seq "Arguments " (:lisp param key)
                                    " and " (:lisp param val)
                                    " direct the generation
    of the table's keys and values, respectively.  For the keys, the
    default element type is \texttt{t} when the test function is
    \texttt{eq} or \texttt{eql}, and \texttt{scalar} otherwise.  For
    the values, the default element type is \texttt{t}."))))
                    (:seq "Beyond those standard Lisp types, NST provides the type "
                          (:inline "scalar")
                          " as a supertype of the numeric types plus "
                          (:lisp type character)
                          ", "
                          (:lisp type string)
                          " and "
                          (:lisp type symbol)
                          ". Users may extend this definition to include additional type specifications, as we discuss below.  Types are not associated with "
                          (:inline "scalar")
                          " are referred to as "
                          (:inline "compound")
                          " (although there is no corresponding type specification).  To avoid generating structures too large to hold in memory, NST provides the global variable "
                          (:lisp variable *max-compound-structure-depth*)
                          " and the macro "
                          (:lisp compiler-macro compund-structure)
                          ".")))

      (collect-symbols :nst (#:*max-compound-structure-depth*
                             #:compound-structure
                             #:def-arbitrary-instance-type)))
    (collect-output (:title "Invariants as tests") ()
      (collect-symbols :keyword (#:sample))))

  (collect-output (:title "Defining test criteria" :short-title "Tests"
                          :leader (:seq "
The criteria used in test forms decide whether, when and how to use
the forms under test and the forms and subcriteria provided to each
test criterion.  Criteria receive their arguments as forms, and may
examine them with or without evaluation, as the particular criterion
requires.  NST provides two mechanisms for defining new criteria, and
a number of support functions for use within these definitions.  The
simpler, but more limited, way to define a new criterion is by
specifying how it should be rewritten to another criterion.  The "
                                        (:lisp compiler-macro
                                               def-criterion-alias)
                                        " macro provides this mechanism, which we discuss in Section "
                                        (:ref def-criterion-alias-section)
                                        ".  The "
                                        (:lisp compiler-macro def-criterion)
                                        " macro provides the more general mechanism for
criteria definition, where Lisp code produces a result report from the
forms under test and criterion's forms and subcriteria.  We discuss "
                                        (:lisp compiler-macro def-criterion)
                                        " in Section "
                                        (:ref def-criterion-section)
                                        ".  We discuss
the NST API for creating these result reports in Section "
                                        (:ref criteria-forms-report-section)
                                        ", and for recursive processing
of subcriteria in Section "
                                        (:ref subcriteria-section)
                                        ".")) ()
    (collect-output (def-criterion-alias-section
                        :title "Aliases over criteria") ()
      (collect-symbols :nst #:def-criterion-alias))
    (collect-output (def-criterion-section :title "Reporting forms"
                      :leader "NST provides functions both for building test reports, and for adding information to a report.")
        (:style 'manual-itemize-style)
      (collect-symbols :nst (#:make-success-report #:make-failure-report
                             #:make-warning-report #:make-error-report
                             #:add-error #:add-failure #:add-info
                             #:add-warning
                             #:wrap-thrown-lisp-warning)))
    (collect-output (criteria-forms-report-section
                     :title "Processing subcriteria"
                     :leader (:seq "The criterion itself can contain "
                                   (:emph "subcriteria")
                                   " which can be
incorporated into the main criterion's assessment.  NST provides two
functions which trigger testing by a subcriterion, each returning the
check's result report.")) ()
(collect-symbols :nst (#:check-criterion-on-value
                       #:check-criterion-on-form)))
    (collect-output (subcriteria-section
                     :title "General criteria definitions") ()
(collect-symbols :nst #:def-criterion)))

  (collect-output (:title "The runtime system"
                          :short-title "Runtime"
                          :leader "The runtime system provides several operations for scheduling and running tests, and debugging failing and erring tests.") ()
    (collect-symbols :nst (#:nst-cmd))
    (collect-doc () (:seq "For the sake of brevity we use the "
                          (:lisp keyword :nst) " shorthand below."))
    (collect-symbols :keyword (#:help))
    (collect-output (:leader "There are a number of commands for running tests, but most of the time only one will be needed: ")
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:run #:run-package #:run-group #:run-test)))
    (collect-output (:leader "One further command for running a test is useful when writing and debugging the tests themselves: ")
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:apply)))
    (collect-output (:leader "There are two commands for (re)printing the results of tests: ")
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:report #:detail)))
    (collect-output (:leader (:seq "The "
                                   (:lisp command :undef)
                                   " and "
                                   (:lisp command :clear)
                                   " commands allow removal of groups, tests and results."))
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:undef #:clear)))
    (collect-output (:leader (:seq "The "
                                   (:lisp command :set)
                                   " and "
                                   (:lisp command :unset)
                                   " display and adjust NST's configuration."))
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:set #:unset)))
    (collect-output (:leader (:seq "There are currently three properties which can be manipulated by "
                                   (:lisp keyword :set) " and "
                                   (:lisp keyword :unset) ":"))
        (:style 'manual-itemize-style)
      (collect-symbols :keyword (#:verbose #:debug-on-error #:debug-on-fail
                                           #:backtraces)))
    (collect-doc () (:seq "The above NST commands are governed by a number of global variables. In general, interactive use of NST should not require direct access to these variables, but when automating NST operations may require changing, or creating a new dynamic scope for, their settings."))
    (collect-output () (:style 'manual-itemize-style)
      (collect-symbols :nst (#:*debug-on-error*
                             #:*debug-on-fail* #:*default-report-verbosity*
                             #:*nst-output-stream*)))
    (collect-doc () (:latex "Fixtures\\index{fixtures!debugging} can be \\emph{opened} into the interactive namespace for debugging with the \\texttt{:nst~:open}
\\\\ Syntax: \\texttt{:nst :open FIXTURE-NAME FIXTURE-NAME ... FIXTURE-NAME}
\\\\ Example:
\\begin{verbatim}
  CL-USER(75): (nst:def-fixtures small-fixture ()
                  (fix-var1 3)
                  (fix-var2 'asdfg))
  NIL
  CL-USER(76): (boundp 'fix-var1)
  NIL
  CL-USER(77): :nst :open small-fixture
  Opened fixture SMALL-FIXTURE.
  CL-USER(78): fix-var1
  3
  CL-USER(79):
\\end{verbatim}
Fixtures can be opened into a different package than where they were
first defined, but these bindings are in addition to the bindings in
the original package, and are made by a symbol import to the
additional package."))
    (collect-doc ()
                 (:seq "Calling " (:lisp keyword :nst) " or "
                       (:lisp compiler-macro nst-cmd)
                       " without a command argument repeats the last test-executing command.")))

  (collect-doc () (:file :latex "doc/asdf.tex" :asdf :nst))

  (aftermatter)
  (collect-output (:style docspec-itemize-latex-style) ()
    (collect-docspec :sift.nst package :style nst-package-list-latex-style))
  (collect-output (:title "Output to JUnit" :short-title "JUnit"
                          :leader "NST reports can be formatted as XML for use with JUnit, although the API for this feature is underdeveloped.") ()
    (collect-symbols :nst (#:junit-results-by-group #:nst-junit-dump)))
  (collect-output (:title "Inheritance-based test methods"
                          :short-title "Methods"
                          :leader (:paragraphs
                                   (:emph "This feature is in-progress. It currently does not work under Lispworks or Clisp, and details of the API may change in subsequent versions.")
                                   "For testing objects in a class hierarchy NST offers xUnit-style test
methods dispatching on different classes.  The idea is that an object
should have all relevant tests applied to it without requiring that
the tests be explicitly enumerated in the test definition: all tests
applicable to an object's class, or to any of its superclasses, should
be discovered and run."
                                   (:seq
                                    "Our running examples of this section are tests on objects of these four classes:"
                                    (:code "\(defclass top-cls ()
     ((tc1 :initarg :tc1 :reader tc1)
      (tc2 :initarg :tc2 :reader tc2)))

\(defclass mid-cls (top-cls)
     ((mc1 :initarg :mc1 :reader mc1)
      (mc2 :initarg :mc2 :reader mc2)))

\(defclass side-cls ()
     ((sc1 :initarg :sc1 :reader sc1)
      (sc2 :initarg :sc2 :reader sc2)))

\(defclass bot-cls (mid-cls side-cls)
     ((bc1 :initarg :bc1 :reader bc1)
      (bc2 :initarg :bc2 :reader bc2)))")))) ()
    (collect-output (:title "Declaring methods"
                            :leader "There are two macros which define a particular method of a generic test function.") ()
      (collect-symbols :nst (#:def-test-method-criterion
                                #:def-test-generic #:def-test-method)))
    (collect-output (:title "Invokng methods") ()
      (collect-symbols :keyword (:methods)))
    (collect-output (:title "Method combinations") ()
      (collect-symbols :nst (#:nst-results))))
  (collect-output (:title "Deprecated forms" :short-title "Deprecated"
                          :leader "The macros, functions and variables documented in this section are all deprecated.  Some continue to be exported from the NST API; others have already been removed.  This section describes how code using these forms should be ported to the active NST API.") ()
    (collect-output (:title "Older criteria-defining macros") ()
      (collect-symbols :nst (#:def-criterion-unevaluated #:def-values-criterion
                                #:def-form-criterion)))
    (collect-output (:title "Old test result generators") ()
      (collect-symbols :nst (#:emit-failure #:emit-success #:emit-warning)))))

(defmethod initialize-instance :around ((o the-manual) &key &allow-other-keys)
  (call-next-method)
  (setf (slot-value o 'contribs)
        (compile-element *package* nil '(:latex "The primary author of both NST and this manual is John Maraist\\footnote{Smart Information Flow Technologies, 211 North First Street, Suite 300, Minneapolis, MN 55401; \\textsl{jmaraist} at \\textsl{sift.info}.}.  Robert P.\\ Goldman provided guidance, comments and suggestions through the development.  Other contributors include Michael J.\\,S.\\ Pelican, Steven A.\\ Harp, Michael Atighetchi and Patrick Stein."))))

(defmethod format-latex-precontents ((style manual-style-mixin)
                                     (item nst-output-toplevel) stream)
  (let ((leader (defdoc-core::output-contents-leader item)))
    (when leader
      (princ "\\noindent " stream)
      (format-output-leader-docspec style leader stream)))
  (call-next-method))

(defmethod format-output-leader-material ((style manual-style-mixin)
                                          stream (output nst-output-toplevel)
                                          &key &allow-other-keys)
  (declare (ignore stream)))
