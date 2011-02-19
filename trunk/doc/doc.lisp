;;; File doc.lisp
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
  (let* ((doc-root-dir (asdf:system-relative-pathname (asdf:find-system :nst)
                                                      "doc/"))
         (gen-dir (merge-pathnames #p"gen/" doc-root-dir))
         (manual-dir (merge-pathnames #p"manual/" doc-root-dir))
         (quickref-dir (merge-pathnames #p"quickref/" doc-root-dir)))
    (format t "Creating documentation in ~a~%" doc-root-dir)
    (defdoc:write-package-specs-latex :nst
        :echo (nst::named-function nst-docs-write-package-specs-latex
                (lambda (&key name type)
                  (format t "Writing ~a ~a for manual...~%" type name)))
        :directory gen-dir
        :style 'nst-item-style
        :include-doctypes '(nst::criterion nst::command nst::switch)
        :package-style 'nst-package-list-latex-style)
    (defdoc:write-package-specs-latex :nst
        :echo (nst::named-function nst-docs-write-package-specs-latex
                (lambda (&key name type)
                  (format t "Writing ~a ~a for quickref...~%" type name)))
        :directory gen-dir
        :style 'nst-quickref
        :include-doctypes '(nst::criterion)
        :package-style nil)
    (defdoc:write-latex-output 'package-api
        :echo (nst::named-function nst-docs-write-latex-output
                (lambda (&key &allow-other-keys)
                  (format t "Writing package-api document spec~%")))
        :directory gen-dir
        :standalone t
        :style 'nst-item-style)

    (defdoc:write-output 'nst-item-style 'the-manual doc-root-dir
                         "new-manual")

    ;; Run LaTeX.
    ;; (format t "Generating new PDF manual from LaTeX...~%")
    ;; (defdoc:process-latex-document manual-dir "new-manual" :index t)
    (format t "Generating PDF manual from LaTeX...~%")
    (defdoc:process-latex-document manual-dir "manual" :index t)
    (format t "Generating quickref from LaTeX...~%")
    (defdoc:process-latex-document quickref-dir "quickref")))

;;; -----------------------------------------------------------------
;;; Style for criteria --- prob. deprecated

(defclass nst-criterion-style (defdoc:latex-style
                               defdoc-control-api:package-list-latex-mixin) ())
(defmethod defdoc-control-api:get-latex-output-file-name ((style nst-criterion-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))

;;; -----------------------------------------------------------------
;;; Formatting callspecs of NST interactive commands and properties.

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::switch))
                                         spec width (calling string))
  (declare (ignore style spec width))
  (cond
   ((< 0 (length calling))
    (concatenate 'string ":nst :set " calling " "))
   (t calling)))

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::command))
                                         spec width (calling string))
  (declare (ignore style spec width))
  (cond
   ((< 0 (length calling))
    (concatenate 'string ":nst " calling " "))
   (t calling)))

(defmethod defdoc-control-api:callspec-prefix (style (target-type (eql 'nst::command))
                                         spec width (calling null))
  (declare (ignore style spec width))
  ":nst")

(defmethod defdoc-control-api:callspec-suffix (style (target-type (eql 'nst::command))
                                         spec width calling)
  (declare (ignore style spec width calling))
  "")

(defmethod defdoc-control-api:callspec-suffix (style (target-type (eql 'nst::switch))
                                         spec width calling)
  (declare (ignore style spec width calling))
  "")

;;; --------------------------------------------------
;;; Style for the manual.

(defclass nst-item-style (defdoc:latex-style) ())
(defmethod defdoc-control-api:get-latex-output-file-name ((style nst-item-style)
                                              usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc-control-api:get-element-for-docspec-format ((style nst-item-style)
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
(defmethod defdoc-control-api:get-element-for-docspec-format ((style nst-item-style)
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
    ((style nst-item-style) (target-type (eql 'nst::criterion))
     (spec defdoc-control-api:standard-doc-spec) stream &key &allow-other-keys)
  (with-accessors ((self defdoc-control-api:docspec-self)) spec
    (format stream "\\subsubsection{The \\texttt{~s} criterion}" self)
    (call-next-method)))

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

;;; --------------------------------------------------
;;; Style for the quickref card.

(defclass nst-quickref (defdoc:latex-style) ())
(defmethod defdoc-control-api:get-latex-output-file-name ((style nst-quickref)
                                                          usage name)
  (string-downcase (concatenate 'string
                     (symbol-name name) "_"
                     (symbol-name usage) "_"
                     (symbol-name (type-of style)) ".tex")))
(defmethod defdoc-control-api:format-docspec-element
    ((style nst-quickref) target-type
     (spec defdoc-control-api:standard-doc-spec) stream &key &allow-other-keys)
  (defdoc-control-api:with-unpacked-standard-spec
      (self intro intro-supp-p params params-supp-p
            short short-supp-p full full-supp-p
            callspec) spec
    ;; (declare (ignore full full-supp-p))
    (cond
     (short-supp-p
      (format-docspec stream style
                      (get-element-for-docspec-format style target-type spec
                                                       :intro short)
                      target-type))
     (intro-supp-p
      (format-docspec stream style
                      (get-element-for-docspec-format style target-type spec
                                                       :intro intro)
                      target-type)))

    (when callspec
      (princ " \\begin{verbatim}" stream)
      (loop for (cs . others) on callspec do
        (loop for line
          in (defdoc-control-api:callspec-to-lines style target-type cs
               defdoc:*latex-verbatim-width* self)
          do
          (format stream "  ~a~%" line))
        (when others (format stream "~%")))
      (princ "\\end{verbatim}" stream))

    (when params-supp-p
      (princ "\\begin{description}" stream)
      (loop for (name subspec) in params do
        (format stream "\\item[~a] " name)
        (format-docspec stream style
                        (get-element-for-docspec-format style target-type spec
                                                         :subspec subspec)
                        target-type))
      (princ "\\end{description}" stream))))

;;; -----------------------------------------------------------------
;;; Starting to debug/use output document specs.

(defdoc:def-output-class (package-api)
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

  (defdoc:collect-groups-by-label
      (nst::api-summary)
    (defdoc:collect-target-type 'function :package :nst)
    (defdoc:collect-target-type 'function :package :nst-control-api)))

(defdoc:def-output-class (the-manual :title "NST 3.0 User Manual"
                                     :leader (:latex "This document is the manual and users' guide to the 3.0.$x$
series of the NST test framework, last updated for 3.0.1.  NST is a
unit test system for Common Lisp which provides support for test
fixture data, stateful setup and cleanup of tests, grouping of tests,
and (we think!) a useful runtime interface.  Suggestions and comments
are welcome.  The files in the NST distribution's \\texttt{self-test}
directory, especially \\texttt{self-test/core/builtin-checks.lisp},
holds the NST tests for NST and contain many examples (some of which
we have adapted for this manual).  Known bugs and infelicities,
platform-specific release notes, and other technical materials are
available via the link on NST's CLiki page, \\textsl{cliki.net/NST}\enspace."))
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
  (collect-output (:title "Tests and test criteria" :short-title "Tests")
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
                                (nst::processes-criteria
                                 :title "Criteria for processes"
                                 :order ())
                                (nst::misc-criteria :title
                                 "Programmatic and debugging criteria"
                                 :order ()))
                      :package :nst)
      (defdoc:collect-target-type 'nst::criterion))
    )
  (collect-output (:title "Defining test criteria" :short-title "Tests"
                          :leader (:latex "
The criteria used in test forms decide whether, when and how to use
the forms under test and the forms and subcriteria provided to each
test criterion.  Criteria receive their arguments as forms, and may
examine them with or without evaluation, as the particular criterion
requires.  NST provides two mechanisms for defining new criteria, and
a number of support functions for use within these definitions.  The
simpler, but more limited, way to define a new criterion is by
specifying how it should be rewritten to another criterion.  The
\\texttt{def-criterion-alias} macro provides this mechanism, which we
discuss in Section~\\ref{sec:def-criterion-alias}.  The
\\texttt{def-criterion} macro provides the more general mechanism for
criteria definition, where Lisp code produces a result report from the
forms under test and criterion's forms and subcriteria.  We discuss
\\texttt{def-criterion} in Section~\\ref{sec:def-criterion}.  We discuss
the NST API for creating these result reports in
Section~\\ref{sec:criteria-forms-report}, and for recursive processing
of subcriteria in Section~\\ref{sec:subcriteria}.
\\par The functions and macros for defining new criteria are exported from
package \\texttt{nst-criteria-api}."))
    (collect-output (:title "Aliases over criteria")
      (collect-symbols :nst #:def-criterion-alias))
    (collect-output (:title "Reporting forms"
                            :leader "NST provides functions both for building test reports, and for adding information to a report.")
      (collect-symbols :nst (#:make-success-report #:make-failure-report
                       #:make-warning-report #:make-error-report
                       #:add-error #:add-failure #:add-info)))
    (collect-output (:title "Processing subcriteria"
                            :leader (:latex "
The criterion itself can contain \\emph{subcriteria} which can be
incorporated into the main criterion's assessment.  NST provides two
functions which trigger testing by a subcriterion, each returning the
check's result report."))
      (collect-symbols :nst (#:check-criterion-on-value
                             #:check-criterion-on-form))))
    (collect-output (:title "General criteria definitions")
      (collect-symbols :nst #:def-criterion))

;;;  (defdoc:collect-groups-by-label
;;;   (nst::nst-manual :groups '(nst::sample
;;;                              nst::runtime
;;;                              nst::asdf)
;;;                       :package :nst)
;;;    (defdoc:collect-exported-symbols :nst)
;;;;;;      (defdoc:collect-target-type 'criterion)
;;;;;;      (defdoc:collect-target-type 'command)
;;;;;;      (defdoc:collect-target-type 'switch)
;;;    )

  )
