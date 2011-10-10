;;; File latex.lisp
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
(in-package :defdoc-latex)

(defgeneric index-lisp-name (style name kind)
  (:method (style name kind) (declare (ignore style name kind)) t)
  (:method (style (name (eql 'symbol)) kind) (declare (ignore style kind)) nil))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :latex (standard-latex :class standard-doc-element
                                    :args (text))
     ((latex :initarg :latex :reader latex-element-latex))
  (make-instance 'standard-latex :latex text))
(set-pprint-dispatch 'standard-latex
  (named-function pprint-standard-latex
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(latex) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))
(defmethod spaceheaded-element ((element standard-latex))
  (whitespace-p (elt (latex-element-latex element) 0)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod output-lines ((style plaintext-style)
                         target-type (spec standard-latex) width)
  (let ((revised (canonicalize-element spec)))
    (output-lines style target-type revised width)))

(defmethod canonicalize-element ((elem standard-latex))
  (let* ((orig-string (latex-element-latex elem))
         (string-chars (loop for spot from (- (length orig-string) 1) downto 0
                           collect (elt orig-string spot))))
    (make-instance 'standard-plain-text
      :text (reassemble-latex-strip string-chars))))

(defun reassemble-latex-strip (input &aux output)
  (loop while input for this = (pop input) do
    (cond
      ;; It's a space.  If it follows whitespace throw it away; else
      ;; keep it.
      ((eql this #\Space)
       (unless (whitespace-p (car input))  (push this output)))

      ;; Other whitespace: convert to a space.
      ((whitespace-p this)
       (push #\Space input))

      ;; It's a backslash, and so is the prior character: that's a
      ;; line break; replace them with a single space.
      ((and (eql this #\\) (eql (car input) #\\))
       (pop input)
       (push #\Space input))

      ;; An isolated backslash
      ((eql this #\\)
       (setf output (check-macro output)))

      ;; Everything else goes onto the output.
      (t (push this output))))

  ;; Coerce that list to a string.
  (coerce (make-array (length output) :initial-contents output) 'string))

(defun check-macro (output-stack)
  (let (macro-name
        (scanner output-stack))
    (loop while (and scanner (alpha-char-p (car scanner))) do
      (push (pop scanner) macro-name))
    (setf macro-name (coerce (reverse macro-name) 'string))
    (loop while (and scanner (whitespace-p (car scanner))) do
      (pop scanner))
    (cond
      ((eql (car scanner) #\{)
       (pop scanner)
       (let ((buffer nil))
         (loop while (and scanner (not (eql (car scanner) #\}))) do
               (push (pop scanner) buffer))
         (pop scanner)
         (cond
          ((or (string= macro-name "index"))
           nil)
          (t
           (loop while buffer do
                 (push (pop buffer) scanner))))
         scanner))
      (t output-stack))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :latex-name (latex-name-element :class standard-doc-element) ()
  (make-instance 'latex-name-element))
(set-pprint-dispatch 'latex-name-element
  (named-function pprint-latex-name-element
    (lambda (stream spec)
      (format stream "[ ~a ]" (type-of spec)))))

(defmethod output-lines (style target-type (spec latex-name-element) width)
  (declare (ignore style target-type width))
  (list "LaTeX"))

(defmethod canonicalize-element ((elem latex-name-element))
    (make-instance 'standard-plain-text :text "LaTeX"))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :bibtex-name (bibtex-name-element :class standard-doc-element) ()
  (make-instance 'bibtex-name-element))
(set-pprint-dispatch 'bibtex-name-element
  (named-function pprint-bibtex-name-element
    (lambda (stream spec)
      (format stream "[ ~a ]" (type-of spec)))))

(defmethod output-lines (style target-type (spec bibtex-name-element) width)
  (declare (ignore style target-type width))
  (list "BibTeX"))

(defmethod canonicalize-element ((elem bibtex-name-element))
    (make-instance 'standard-plain-text :text "BibTeX"))

;;; -----------------------------------------------------------------
;;; Lisp macros for defining related families of LaTeX-related generic
;;; functions.

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Functions corresponding to LaTeX length commands.

(defpackage :defdoc-latex-contextualized)

(defvar *latex-length-commands* (make-hash-table :test 'eq)
  "Set containing the symbols naming LaTeX langth commands defined using the def-latex-length-generic macro.")
(defgeneric get-latex-length-generic-macroname (lisp-fname))
(defmacro def-latex-length-generic (lisp-fname latex-macroname
                                               &rest default-and-contexts)
  (let ((style (gensym)) (item (gensym)) (context (gensym))
        (contextual-fname (intern (symbol-name lisp-fname)
                                  (find-package :defdoc-latex-contextualized))))
    (when (null default-and-contexts)
      (setf default-and-contexts '(nil)))
    (destructuring-bind (default . contexts) default-and-contexts
      `(progn
         (setf (gethash ',lisp-fname *latex-length-commands*) t)
         (defgeneric ,lisp-fname (,style ,item)
           (:method (,style ,item)
             (declare (ignore ,style ,item))
             ,default))
         (defgeneric ,contextual-fname (,style ,context ,item)
           (:method (,style ,context ,item)
             (declare (ignore ,style ,item ,context))
             nil))
         (defmethod get-latex-length-generic-macroname ((fn (eql ',lisp-fname)))
           ,latex-macroname)
         ,@(loop for (context-name value) in contexts
                 collect `(defmethod ,contextual-fname
                              (,style (,context (eql ,context-name)) ,item)
                            (declare (ignore ,style ,item))
                            ,value))))))

(def-latex-length-generic latex-parskip "parskip")
(def-latex-length-generic latex-parindent "parindent")

(defgeneric format-latex-global-length-commands (style item stream)
  (:method (style item stream)
    (loop for fname being the hash-keys of *latex-length-commands* do
      (let ((value (funcall fname style item)))
        (when value
          (format stream "\\~a ~a~%"
            (get-latex-length-generic-macroname fname) value))))))

(defgeneric format-latex-docspec-header-commands (style item stream)
  (:method (style item stream)
    (declare (ignore style item))
;;;    (format stream "~&\\newcommand{\\docspecHeader}[3]{
;;;\\begin{trivlist}
;;;\\bfseries\\itshape\\mbox{#1 #2}\\hspace*{\\fill}\\mbox{#3}
;;;\\end{trivlist}
;;;\\vspace*{-1\\parskip}
;;;}~&")

    (princ "%" stream)
    (terpri stream)
    (princ "\\makeatletter" stream)
    (terpri stream)
    (princ "\\newcommand{\\docspecHeader}[3]{%" stream)
    (terpri stream)
    (princ "  \\@startsection{paragraph}{4}{0pt}{0pt}{0.01em}{\\bfseries\\itshape}%" stream)
    (terpri stream)
    (princ "{\\mbox{#1 #2}\\hspace*{\\fill}\\mbox{#3}}%" stream)
    (terpri stream)
    (princ "\\vskip -3.5ex\\hrulefill\\\\}" stream)
    (terpri stream)
    (princ "\\makeatother" stream)
    (terpri stream)))

(defgeneric format-latex-local-length-commands (style context item stream)
  (:method (style context item stream)
    (loop for fname being the hash-keys of *latex-length-commands* do
      (let* ((contextual-fname
              (intern (symbol-name fname)
                      (find-package :defdoc-latex-contextualized)))
             (value (funcall contextual-fname style context item)))
        (when value
          (format stream "\\~a ~a~%"
            (get-latex-length-generic-macroname fname) value))))))

;;; -----------------------------------------------------------------
;;; Top-level LaTeX generation APIs.

(defvar *latex-sectioning-level* -1)
(defvar *defdoc-latex-default-directory* "./")
(defvar *latex-full-package-item-header-macro* "\\paragraph")
(defvar *latex-verbatim-width* 57)
(defvar *latex-generate-index* nil)
(defvar *latex-generate-toc* nil)
(defvar *latex-default-documentclass* "article")
(defvar *latex-default-usepackage-specs*
    '(times helvet array (hyperref pdftex)))
(defvar *default-primary-tocdepth* nil)
(defvar *default-secnumdepth* nil)

(defgeneric get-latex-document-class (style item)
  (:method (style item)
    (declare (ignore style item))
    (values *latex-default-documentclass* nil)))

(defgeneric get-latex-usepackage-specs (style item)
  (:method (style item)
    (declare (ignore style item))
    *latex-default-usepackage-specs*))

(defgeneric get-latex-primary-tocdepth (style item)
  (:method (style item)
    (declare (ignore style item))
    *default-primary-tocdepth*))

(defgeneric get-latex-secnumdepth (style item)
  (:method (style item)
    (declare (ignore style item))
    *default-secnumdepth*))


(defun format-command-with-optional-args (stream command arg opt-args)
  (format stream "\\~a" command)
  (cond
    ((or (null opt-args) (eq t opt-args)))
    ((listp opt-args)
     (format stream "[~{~a~^,~}]" opt-args))
    (t
     (format stream "[~a]" opt-args)))
  (format stream "{~a}" arg))

(defun format-usepackage-specs (stream specs)
  (loop for spec in specs do
    (format stream "~%")
    (cond
      ((listp spec)
       (format-command-with-optional-args stream "usepackage"
                                          (car spec) (cdr spec)))
      (t (format-command-with-optional-args stream "usepackage" spec nil)))))

(defgeneric get-latex-output-file-name (style usage name)
  (:method ((style symbol) usage name)
           (concatenate 'string
             (symbol-name name) "_"
             (symbol-name usage) "_"
             (symbol-name style) ".tex"))
  (:method (style usage name)
     (get-latex-output-file-name (type-of style) usage name)))

(defgeneric format-latex-standalone-header (style stream out &optional
                                                             contents index)
  (:method (style stream (out output-contents) &optional contents index)
    (multiple-value-bind (class class-args)
        (get-latex-document-class style out)
      (format-command-with-optional-args stream
                                         "documentclass" class class-args))
    (format-usepackage-specs stream (get-latex-usepackage-specs style out))
    (when index
      (format stream "~%\\usepackage{makeidx}~%\\makeindex~%"))
    (let ((title (get-output-unit-title out))
          (author (get-output-unit-author out)))
      (when (or title author)
        (cond
          (title (princ "\\title{" stream)
                 (format-docspec-element style nil title stream)
                 (format stream "}~%"))
          (t (format stream "\\title{}~%")))
        (cond
          (author (format stream "\\author{")
                  (format-docspec-element style nil author stream)
                  (format stream "}~%"))
          (t (format stream "\\author{}~%"))))
      (let ((primary-tocdepth (get-latex-primary-tocdepth style out)))
        (when primary-tocdepth
          (format stream "\\addtocontents{toc}{\\setcounter{tocdepth}{~d}}~%"
            primary-tocdepth)))
      (let ((secnumdepth (get-latex-secnumdepth style out)))
        (when secnumdepth
          (format stream "\\setcounter{secnumdepth}{~d}~%" secnumdepth)))
      (format stream "\\begin{document}~%")
      (when (or title author)
        (format stream "\\maketitle~%"))
      (format-latex-precontents style out stream)
      (when contents
        (format-latex-local-length-commands style :toc out stream)
        (format stream "\\tableofcontents~%"))
      (format-latex-global-length-commands style out stream)
      (format-latex-docspec-header-commands style out stream))))

(defgeneric format-latex-precontents (style item stream)
  (:method (style out stream)
    (declare (ignore style out stream))))

(defgeneric format-latex-standalone-footer (style stream out &optional index)
  (:method (style stream (out output-contents) &optional index)
     (declare (ignore style))
     (when index (format stream "\\printindex~%"))
     (format stream "\\end{document}~%")))

(defun write-latex-output (name &key
                                (table-of-contents nil)
                                (index nil)
                                (echo (named-function write-latex-output-nop
                                        (lambda ())))
                                (style 'latex-style)
                                (directory #p"./")
                                (file nil file-supp-p)
                                standalone)
  (when (symbolp style)
    (setf style (make-instance style)))
  (let ((output-framework (make-instance name))
        (*latex-generate-index* index)
        (*latex-generate-toc* table-of-contents))
    (declare (special *latex-generate-index**latex-generate-toc*))
    (unless output-framework
      (error "No such output framework ~s" name))
    (unless file-supp-p
      (setf file (format nil "~a_~a.tex" name (type-of style))))

    (funcall echo)
    (let ((file-spec (merge-pathnames file directory)))
      (with-open-file (out file-spec :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
        (when standalone
          (format-latex-standalone-header style out output-framework
                                          table-of-contents index))
        (format-doc out style output-framework)
        (when standalone
          (format-latex-standalone-footer style out output-framework index))))))

(defun write-spec-latex (name target-type &key
                              (style 'latex-style)
                              (directory *defdoc-latex-default-directory*)
                              (file nil file-supp-p))
  ;; (format t "----------~%style ~s~%dir ~s~%file ~s~%" style directory file)
  (when (symbolp style)
    (setf style (make-instance style)))

  (unless file-supp-p
    (setf file (get-latex-output-file-name style target-type name)))

  #+(or sbcl lispworks)
  (setf file (block quote-wild
               (loop for char across file
                   append (case char
                            ((#\* #\? #\[ #\]) (list #\\ char))
                            (otherwise (list char))) into new-file
                   finally (return-from quote-wild
                             (coerce new-file 'string)))))

;;;  #+clozure
;;;  (progn
;;;    (format t "Correcting ~s for Clozure~%" file)
;;;    (setf file (block quote-wild
;;;                 (loop for char across file
;;;                     append (case char
;;;                              ((#\* #\? #\[ #\]) (list #\\ #\\ #\\ char))
;;;                              (otherwise (list char))) into new-file
;;;                     finally (return-from quote-wild
;;;                               (coerce new-file 'string)))))
;;;    (format t "Corrected to ~s~%" file))

  (let ((file-spec (merge-pathnames file directory)))
    (with-open-file (out file-spec
                     :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
      (format-doc out style (get-doc-spec name target-type)))))

(defun write-doctype-latex (doctype &key (echo nil echo-supp)
                                    (dir *defdoc-latex-default-directory*)
                                    (style 'latex-style))
  (loop for name being the hash-keys of (get-doc-hash-of-target-type doctype) do
    (when echo-supp
      (funcall echo :name name :type doctype))
    (write-spec-latex name doctype :directory dir :style style)))

(defun write-package-specs-latex (package-specifier
                                  &key (echo nil echo-supp)
                                  (directory *defdoc-latex-default-directory*)
                                  (style 'latex-style)
                                  (package-style t)
                                  include-doctypes)
  (when (eq package-style t)
    (setf package-style style))

  (let ((package (find-package package-specifier)))
    (do-external-symbols (sym package)
      (loop for form in (get-doc-target-types) do
        (when (get-doc-spec sym form)
          (when echo-supp
            (funcall echo :name sym :type form))
          (write-spec-latex sym form :directory directory :style style))))

    (loop for doctype in include-doctypes do
      (loop for sym being the hash-keys
            of (get-doc-hash-of-target-type doctype) do
        (when (get-doc-spec sym doctype)
          (when echo-supp
            (funcall echo :name sym :type doctype))
          (write-spec-latex sym doctype :directory directory :style style))))

    ;; (format t "package-style ~s~%" package-style)
    (when package-style
      (let ((pkg-sym-name (intern (package-name package) :keyword)))
        ;; (format t "package-sym ~s~%" pkg-sym-name)
        (when (get-doc-spec pkg-sym-name 'package)
          (when echo-supp (funcall echo :name pkg-sym-name :type 'package))
          (write-spec-latex pkg-sym-name 'package
                            :directory directory :style package-style)
          (unless (eq package-style style)
            (write-spec-latex pkg-sym-name 'package
                              :directory directory :style style)))))))

;;; -----------------------------------------------------------------
;;; The default LaTeX style.

(defclass latex-style () ())

(defgeneric display-latex-section (output)
  (:method (o)
     (declare (ignore o))
     nil)
  (:method ((o output-contents))
     (> *latex-sectioning-level* 0))
  (:method ((o grouped-output-contents))
    (with-accessors ((label get-grouped-output-labeldef)
                     (group get-grouped-output-group)) o
      (or (get-label-section-title-supp-p label group o)
          (call-next-method))))
  (:method :around (o)
     (when (get-output-unit-title o)
       (call-next-method))))

;;;(defgeneric has-latex-section-title (output)
;;;  (:method (o)
;;;     (declare (ignore o))
;;;     nil)
;;;  (:method ((o output-contents))
;;;     (get-output-unit-title o))
;;;  (:method ((o grouped-output-contents))
;;;    (with-accessors ((label get-grouped-output-labeldef)
;;;                     (group get-grouped-output-group)) o
;;;      (or (get-label-section-title-supp-p label group o)
;;;          (call-next-method)))))

(defmethod format-doc :around (stream (style latex-style) output
                                      &key &allow-other-keys)
  (declare (ignore stream))
  (let ((*latex-sectioning-level* (cond
                                    ((get-output-unit-title output)
                                     ;; (has-latex-section-title output)
                                     (+ 1 *latex-sectioning-level*))
                                    (t *latex-sectioning-level*))))
    (declare (special *latex-sectioning-level*))
    (call-next-method)))

(defvar *aftermatter-tocdepth* nil)
(defmethod format-docspec-aftermatter-mark ((style latex-style) mark stream
                                             &key &allow-other-keys)
  (declare (ignore mark))
  (princ "\\appendix " stream)
  (when *aftermatter-tocdepth*
    (format stream "\\addtocontents{toc}{\\setcounter{tocdepth}{~d}}"
      *aftermatter-tocdepth*)))

(defmethod format-sequence-element-separator ((style latex-style)
                                              target-type spec element1 element2
                                              stream &key &allow-other-keys)
  (declare (ignore target-type spec element1))
  (when (spaceheaded-element element2)
    (princ "\\ " stream)))

(defgeneric format-latex-pre-output-leader-material (style stream output
                                                           &key
                                                           &allow-other-keys)
  (:method (style stream output &key &allow-other-keys)
    (declare (ignore style))
    (when (and *latex-generate-toc* (eql *latex-sectioning-level* 0)
               (get-output-unit-title output))
      (format stream "\\vspace*{1em}"))))

(defmethod format-output-leader-material :before ((style latex-style)
                                                  stream output &rest keyvals)
  (apply #'format-latex-pre-output-leader-material style stream output keyvals))

(defgeneric format-latex-post-output-leader-material (style stream output
                                                            &key
                                                            &allow-other-keys)
  (:method (style stream output &key &allow-other-keys)
    (declare (ignore style stream output)))
  (:method (style stream (output output-contents) &key &allow-other-keys)
    (declare (ignore style))
    (format stream "\\label{~a}" (type-of output))))

(defmethod format-output-leader-material :after ((style latex-style)
                                                 stream output &rest keyvals)
  (apply #'format-latex-post-output-leader-material
         style stream output keyvals))

(defmethod format-default-output-contents-sep ((style latex-style)
                                               stream output spec1 spec2)
  (declare (ignore output spec1 spec2))
  (princ " " stream))

(defmethod format-docspec :before (stream (style latex-style)
                                          (spec doc-spec) target-type
                                          &key &allow-other-keys)
  (declare (ignore target-type))
  (when *latex-generate-index*
    (format stream "\\index{~a@\\texttt{~:*~a}|(}" (docspec-self spec))))

(defmethod format-docspec :after (stream (style latex-style)
                                         (spec doc-spec) target-type
                                         &key &allow-other-keys)
  (declare (ignore target-type))
  (when *latex-generate-index*
    (format stream "\\index{~a@\\texttt{~:*~a}|)}" (docspec-self spec))))

(defmethod get-default-callspec-block-width ((style latex-style)
                                             target-type spec)
  (declare (ignore target-type spec))
  *latex-verbatim-width*)

(defmethod format-standard-docspec-literal-text ((style latex-style)
                                                 text stream
                                                 &key &allow-other-keys)
  (format stream "\\begin{verbatim}~a\\end{verbatim}" text))

(defmethod format-standard-docspec-param-list-start ((style latex-style)
                                                       type spec stream
                                                       &key &allow-other-keys)
  (declare (ignore type spec))
  (princ "\\begin{description}" stream))

(defmethod format-standard-docspec-param-list-stop ((style latex-style)
                                                      type spec stream
                                                      &key &allow-other-keys)
  (declare (ignore type spec))
  (princ "\\end{description}" stream))

(defmethod format-standard-docspec-param-list-item-start
    ((style latex-style) type spec name stream &key &allow-other-keys)
  (declare (ignore type spec))
  (format stream "\\item[~a] " name))

(defmethod format-standard-docspec-param-list-item-stop
    ((style latex-style) type spec name stream &key &allow-other-keys)
  (declare (ignore stream name type spec)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-plain-text) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (with-accessors ((string text-element-text)) spec
    (let ((was-space t))
      (loop for char across string
          append (let ((next-space nil))
                   (prog1
                       (case char
                         (#\\ (coerce "$\\backslash$" 'list))
                         ((#\$ #\% #\# #\& #\_) (list #\\ char))
                         ((#\~ #\^) (list #\\ char #\{ #\}))
                         ((#\")
                          (cond
                           (was-space (list #\` #\`))
                           (t (list #\' #\'))))
                         ((#\space #\tab)
                          (setf next-space t) (list #\space))
                         (otherwise (list char)))
                     (setf was-space next-space)))
          into characters
          finally (princ (coerce characters 'string) stream)))))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-latex) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (princ (latex-element-latex spec) stream))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-paragraph-list) stream
                                   &key &allow-other-keys)
  (loop for (p-spec . other-specs) on (paragraphlist-element-items spec) do
    (format-docspec stream style p-spec target-type)
    (when other-specs (princ "\\par " stream))))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-code) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "\\begin{verbatim}~a\\end{verbatim}" (code-element-string spec)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-inline) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "\\texttt{~a}" (inline-element-string spec)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-simple-list-environment)
                                   stream
                                   &key &allow-other-keys)
  (format stream "\\begin{~a}" (list-element-env-tag spec))
  (loop for spec in (list-element-specs spec) do
    (princ "\\item " stream)
    (format-docspec stream style spec target-type))
  (format stream "\\end{~a}" (list-element-env-tag spec)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-lisp-name) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (let ((name (lisp-name spec))
        (kind (lisp-name-kind spec)))
    (when (index-lisp-name style name kind)
      (format stream "\\index{~a@\\texttt{~:*~a}}" name))
    (format stream "\\texttt{~a}" name)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-reference) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (let ((name (referenced-name spec)))
    (format stream "\\ref{~a}" name)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-emphasized) stream
                                   &rest keyvals)
  (format stream "\\emph{")
  (apply #'format-docspec-element
         style target-type (emphasized-spec spec) stream keyvals)
  (format stream "}"))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (name latex-name-element) stream
                                   &key in-seq &allow-other-keys)
  (declare (ignore target-type))
  (format stream "\\LaTeX")
  (when in-seq (format stream "\\ ")))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (name bibtex-name-element) stream
                                   &key in-seq &allow-other-keys)
  (declare (ignore target-type))
  (format stream "Bib$\\!$\\TeX")
  (when in-seq (format stream "\\ ")))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (name standard-fillin-place) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "\\fbox{FILL IN}"))

(defgeneric latex-section-formatter (style sectioning-level)
  (:method (style level)
     (declare (ignore style))
     (case level
       ((1) "\\section{~a}")
       ((2) "\\subsection{~a}")
       ((3) "\\subsubsection{~a}")
       ((4) "\\subsubsection*{~a}")
       ((5) "\\paragraph{~a}")
       ((6) "\\subparagraph{~a}")
       (otherwise (error "Too deeply grouped.")))))

(defmethod format-output-leader-title :around ((style latex-style) stream
                                               (output output-contents)
                                               &key &allow-other-keys)
  (declare (ignore stream) (special *latex-sectioning-level*))
  (when (> *latex-sectioning-level* 0)
    (let ((*output-leader-title-format-string*
           (latex-section-formatter style *latex-sectioning-level*)))
      (declare (special *output-leader-title-format-string*))
      (call-next-method))))

(defmethod format-output-leader-sep ((style latex-style) stream output)
  (declare (ignore output))
  (princ "\\par " stream))

;;; -----------------------------------------------------------------
;;; Mixin for a full description of packages.

(defclass full-package-latex-style-mixin () ())

(defmethod format-docspec (stream (style full-package-latex-style-mixin)
                                  (spec standard-doc-spec)
                                  (target-type (eql 'package))
                                  &key &allow-other-keys)
  (with-accessors ((self docspec-self)) spec
    (call-next-method)
    (do-external-symbols (var (find-package self))
      (format stream "~a{~a}" *latex-full-package-item-header-macro* var)
      (loop for target-type in (get-doc-target-types var) do
        (format-docspec stream style
                               (get-doc-spec var target-type) target-type)))))

;;; -----------------------------------------------------------------
;;; Style for a sectioned listing of the symbols exported in a
;;; package.

(defgeneric package-list-overall-header (style spec stream)
  (:method (style spec stream)
     (declare (ignore style))
     (format stream "\\section{The ~a API}~%" (docspec-descriptive spec))))
(defgeneric package-list-group-header (style spec group stream)
  (:method (style spec group stream)
     (princ "\\subsection{" stream)
     ;; (format t "~s ~s~%" self group)
     (format-tag style (find-package (docspec-self spec)) group stream)
     (format stream "}~%")))
(defgeneric package-list-entry (style spec group entry stream)
  (:method (style spec group entry stream)
     (declare (ignore style spec group))
     (format stream "\\texttt{~a}~%~%" (docspec-self entry))))

(defclass package-list-latex-mixin () ())

(defmethod format-docspec (stream (style package-list-latex-mixin)
                           (spec standard-doc-spec) (target-type (eql 'package))
                           &key &allow-other-keys)
  (with-accessors ((self docspec-self)) spec
    (let ((actual-package (find-package self))
          (tag-sort (make-hash-table :test 'eq))
          (pspec (get-doc-spec self target-type)))
      (do-external-symbols (sym self)
        (loop for type in (get-doc-target-types sym) do
          (loop for tag in (get-doc-tags sym type) do
            (let ((tag-hash (gethash tag tag-sort)))
              (unless tag-hash
                (setf tag-hash (make-hash-table :test 'eq)
                      (gethash tag tag-sort) tag-hash))
              (push type (gethash sym tag-hash))))))

      (package-list-overall-header style pspec stream)
      (let ((tag-list
             (loop for tag being the hash-keys of tag-sort collect tag)))
        (loop for tag
            in (sort tag-list
                     (named-function format-docspec-element-tag-sorter
                       (lambda (x y)
                         (let ((sx (tag-sort style actual-package x))
                               (sy (tag-sort style
                                             actual-package y)))
                           (< sx sy)))))
            for tag-hash = (gethash tag tag-sort)
            do
              (package-list-group-header style pspec tag stream)
              (let ((names
                     (loop for name being the hash-keys of tag-hash collect name)))
                (loop for name in (sort names #'string-lessp)
                    for type-list = (gethash name tag-hash)
                    do
                      (loop for type in type-list do
                            (package-list-entry style
                                                pspec tag (get-doc-spec name type) stream)))))))))

;;; -----------------------------------------------------------------

(defmethod write-output ((style latex-style) output-name directory file-name
                         &rest keyargs
                         &key index table-of-contents &allow-other-keys)
  (write-latex-output output-name
                      :echo #'(lambda (&key &allow-other-keys)
                                (format t "Writing ~a~%" output-name))
                      :directory directory
                      :file (format nil "~a~a"
                              file-name
                              (apply #'get-filename-extension
                                     style output-name directory file-name
                                     keyargs))
                      :standalone t
                      :index index :table-of-contents table-of-contents
                      :style style)
  (process-latex-document directory file-name :index index))

(defmethod get-filename-extension ((style latex-style)
                                   output-name directory file-name-root
                                   &key &allow-other-keys)
  (declare (ignore output-name directory file-name-root))
  ".tex")

;;; -----------------------------------------------------------------

(defclass docspec-par-latex-style () ())

(defmethod format-output-contents-sep ((style docspec-par-latex-style)
                                       stream output spec1 spec2
                                       &key &allow-other-keys)
  (declare (ignore output spec1 spec2))
  (format stream "\\par "))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defclass docspec-list-latex-style ()
  ((list-name :reader docspec-list-environment-name)))

(defmethod format-output-leader-material ((style docspec-list-latex-style)
                                          stream output &key &allow-other-keys)
  (declare (ignore output))
  (call-next-method)
  (format stream "\\begin{~a}" (docspec-list-environment-name style)))
(defmethod format-output-trailer-material ((style docspec-list-latex-style)
                                           stream output &key &allow-other-keys)
  (declare (ignore output))
  (format stream "\\end{~a}" (docspec-list-environment-name style))
  (call-next-method))
(defmethod format-output-preitem ((style docspec-list-latex-style)
                                  stream output item &key &allow-other-keys)
  (declare (ignore output item))
  (princ "\\item " stream)
  (call-next-method))

(defclass docspec-itemize-latex-style (docspec-list-latex-style) ())
(defmethod initialize-instance :after ((o docspec-itemize-latex-style)
                                       &key &allow-other-keys)
  (setf (slot-value o 'list-name) "itemize"))

(defclass docspec-enumerate-latex-style (docspec-list-latex-style) ())
(defmethod initialize-instance :after ((o docspec-enumerate-latex-style)
                                       &key &allow-other-keys)
  (setf (slot-value o 'list-name) "enumerate"))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defclass docspec-fancy-header-latex-style (docspec-par-latex-style)
  ())

(defgeneric default-format-fancy-header-target-type (style target-type
                                                           spec name stream)
  (:method (style target-type spec name stream)
    (declare (ignore spec name style))
    (princ (capitalized-target-name (get-target-type target-type)) stream)))

(defgeneric format-fancy-header-target-type (style target-type spec name stream)
  (:method ((style docspec-fancy-header-latex-style)
            target-type spec name stream)
    (default-format-fancy-header-target-type style target-type
      spec name stream))
  (:method ((style docspec-fancy-header-latex-style)
            (target-type (eql 'function)) spec name stream)
    (declare (ignore spec))
    (cond
     ((and (fboundp name) (typep (symbol-function name) 'generic-function))
      (princ "Generic function" stream))
     (t (call-next-method)))))

(defmethod format-docspec
    :before (stream (style docspec-fancy-header-latex-style)
                    (spec standard-doc-spec) target-type &key &allow-other-keys)
  (let ((self (docspec-self spec)))
    (multiple-value-bind (home-package exported-p)
        (locate-package-home style target-type spec self)

      (format stream "%~&\\docspecHeader{")
      (format-fancy-header-target-type style target-type spec self stream)
      (format stream "}{~a}{~a}~&"
        (symbol-name self)
        (format nil (if exported-p "Package :~a" "Internal to :~a")
          (package-name home-package)))

;;;      (format stream "%~&\\begin{trivlist}%~&  {\\bfseries\\itshape\\mbox{")
;;;      (format-fancy-header-target-type style target-type spec self stream)
;;;      (format stream " ~a}\\hspace*{\\fill}\\mbox{~a :~a}}%~&\\end{trivlist}%~&\\vspace*{-1\\parskip}%~&"
;;;              (symbol-name self)
;;;              (if exported-p "Package" "Internal to")
;;;              (package-name home-package))

;;;      (format stream "%~&\\@startsection{paragraph}{4}{0pt}{0pt}{0.05em}{")
;;;      (format-fancy-header-target-type style target-type spec self stream)
;;;      (format stream " ~a\\hspace*{\\fill}\\mbox{~a :~a}}%~&"
;;;              (symbol-name self)
;;;              (if exported-p "Package" "Internal to")
;;;              (package-name home-package))

      )))

(defmethod format-standard-docspec-details-sep ((s latex-style) type spec stream
                                                &key &allow-other-keys)
  (declare (ignore type spec))
  (format stream "\\par "))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Methods for LaTeX styles that uses an {itemize} for organizing the
;;; specs.

(defmethod format-itemized-list-start ((style latex-style) stream)
  (format stream "\\begin{itemize}"))
(defmethod format-itemized-list-end ((style latex-style) stream)
  (format stream "\\end{itemize}"))
(defmethod format-itemized-list-item-start ((style latex-style) stream)
  (format stream "\\item "))
(defmethod format-itemized-list-item-end ((style latex-style) stream)
  (declare (ignore stream)))

;;; -----------------------------------------------------------------

(defun process-latex-document (directory-path bare-name &key bibtex index)
  (labels ((set-dir ()
             #+allegro (excl:chdir directory-path)
             #+sbcl (sb-posix:chdir directory-path)
             #+clozure (setf (ccl:current-directory) directory-path)
             #+clisp (ext:cd directory-path)
             #+lispworks (hcl:change-directory directory-path)
             #-(or allegro sbcl clozure
                   clisp lispworks) (error "Not implemented on this system."))
           (run-command (executable &rest args)
             #+allegro (unless (eql 0 (excl:run-shell-command
                                       (format nil "~a~{ ~a~}"
                                         executable args)))
                         (error "excl:run-shell-command failed: ~a~{ ~a~}"
                                executable args))
             #+sbcl (let (exit-code)
                      (let ((output
                             (with-output-to-string (str)
                               (setf exit-code
                                 (sb-ext:process-exit-code
                                  (sb-ext:run-program executable args
                                                      :wait t
                                                      :search t
                                                      :output str
                                                      :error str))))))
                        (unless (eql exit-code 0)
                          (error "excl:run-shell-command failed: ~a~{ ~a~}~%~a"
                                 executable args output))))
;;;           #+clozure (run-program executable args
;;;                                  :wait t :search t :output nil :error t)
;;;           #+clisp (ext:shell (format nil (format nil "~a~{ ~a~}"
;;;                                            executable args) bare-name))
;;;           #+lispworks (system:call-system
;;;                        (format nil (format nil "~a~{ ~a~}"
;;;                                      executable args) bare-name))
             #-(or allegro sbcl) (error "Not implemented on this system."))
           (run-latex ()
             (run-command "pdflatex" "-interaction=batchmode"
                          (format nil "~a.tex" bare-name)))
           (run-bibtex ()     (run-command "bibtex" "-terse" bare-name))
           (run-makeindex ()  (run-command "makeindex" "-terse" bare-name)))

    (set-dir)
    (run-latex)
    (when bibtex (run-bibtex))
    (when index (run-makeindex))
    (when (or bibtex index) (run-latex))
    (run-latex)))

;;; -----------------------------------------------------------------

(defmacro def-latex-style-class (name superclasses fields
                                      (&rest keyvals &key
                                             (usepackage nil usepackage-supp-p)
                                             (secnumdepth nil
                                                          secnumdepth-supp-p)
                                             (primary-tocdepth
                                              nil primary-tocdepth-supp-p)
                                             (contextualized-parskip
                                              nil contextualized-parskip-supp-p)
                                             (parskip nil parskip-supp-p)
                                             (parindent nil parindent-supp-p)
                                             &allow-other-keys)
                                      &body class-forms)
  `(progn
     (def-standard-style-class ,name ,superclasses ,fields ,keyvals
                               ,@class-forms)
     ,@(when usepackage-supp-p
         `((defmethod get-latex-usepackage-specs ((style ,name) item)
             (declare (ignorable item))
             ,usepackage)))
     ,@(when primary-tocdepth-supp-p
         `((defmethod get-latex-primary-tocdepth ((style ,name) item)
             (declare (ignorable item))
             ,primary-tocdepth)))
     ,@(when secnumdepth-supp-p
         `((defmethod get-latex-secnumdepth ((style ,name) item)
             (declare (ignorable item))
             ,secnumdepth)))
     ,@(when parskip-supp-p
         `((defmethod latex-parskip ((style ,name) item)
             (declare (ignorable item))
             ,parskip)))
     ,@(when contextualized-parskip-supp-p
         (loop for (context form) in contextualized-parskip
               collect
               `(defmethod defdoc-latex-contextualized::latex-parskip
                    ((style ,name) (context (eql ,context)) item)
                  (declare (ignorable item))
                  ,form)))
     ,@(when parindent-supp-p
         `((defmethod latex-parindent ((style ,name) item)
             (declare (ignorable item))
             ,parindent)))))

