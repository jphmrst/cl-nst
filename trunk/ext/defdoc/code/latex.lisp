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

;;; -----------------------------------------------------------------
;;; Top-level LaTeX generation APIs.

(defvar *latex-sectioning-level* -1)
(defvar *defdoc-latex-default-directory* "./")
(defvar *latex-full-package-item-header-macro* "\\paragraph")
(defvar *latex-verbatim-width* 57)
(defvar *latex-generate-index* nil)
(defvar *latex-generate-toc* nil)
(defvar *latex-default-header-matter*
    "\\documentclass{article}
\\usepackage{times}
\\usepackage{helvet}
\\usepackage[pdftex]{hyperref}")

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
     (format stream "~a" *latex-default-header-matter*)
     (when index
       (format stream "\\usepackage{makeidx}~%\\makeindex~%"))
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
          (t
           (format stream "\\author{}~%"))))
       (format stream "\\begin{document}~%")
       (when (or title author)
         (format stream "\\maketitle~%"))
       (when contents
         (format stream "\\tableofcontents~%")))))

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

(defmethod format-output-leader-material :before ((style latex-style)
                                                  stream output
                                                  &key &allow-other-keys)
  (when (and *latex-generate-toc* (eql *latex-sectioning-level* 0)
             (get-output-unit-title output))
    (format stream "\\vspace*{1em}")))

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
                                   (spec standard-plain-text) stream)
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
                                   (spec standard-latex) stream)
  (declare (ignore target-type))
  (princ (latex-element-latex spec) stream))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-paragraph-list) stream)
  (loop for (p-spec . other-specs) on (paragraphlist-element-items spec) do
    (format-docspec stream style p-spec target-type)
    (when other-specs (princ "\\par " stream))))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-sequence) stream)
  (loop for (p-spec . other-specs) on (sequence-element-items spec) do
    (format-docspec stream style p-spec target-type)
    (when other-specs (princ " " stream))))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-code) stream)
  (declare (ignore target-type))
  (format stream "\\begin{verbatim}~a\\end{verbatim}" (code-element-string spec)))

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-simple-list-environment)
                                   stream)
  (format stream "\\begin{~a}" (list-element-env-tag spec))
  (loop for spec in (list-element-specs spec) do
    (princ "\\item " stream)
    (format-docspec stream style spec target-type))
  (format stream "\\end{~a}" (list-element-env-tag spec)))

(defgeneric latex-section-formatter (style sectioning-level)
  (:method (style level)
     (declare (ignore style))
     (case level
       ((1) "\\section{~a}")
       ((2) "\\subsection{~a}")
       ((3) "\\subsubsection{~a}")
       ((4) "\\paragraph{~a}")
       ((5) "\\subparagraph{~a}")
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
                         &key index table-of-contents &allow-other-keys)
  (write-latex-output output-name
                      :echo #'(lambda (&key &allow-other-keys)
                                (format t "Writing ~a~%" output-name))
                      :directory directory
                      :file (format nil "~a.tex" file-name)
                      :standalone t
                      :index index :table-of-contents table-of-contents
                      :style style)
  (process-latex-document directory file-name :index index))

;;; -----------------------------------------------------------------

(defun process-latex-document (directory-path bare-name &key bibtex index)
  (flet ((set-dir ()
           #+allegro (excl:chdir directory-path)
           #+sbcl (sb-posix:chdir directory-path)
           #+clozure (setf (ccl:current-directory) directory-path)
           #+clisp (ext:cd directory-path)
           #+lispworks (hcl:change-directory directory-path)
           #-(or allegro sbcl clozure
                 clisp lispworks) (error "Not implemented on this system."))
         (run-latex ()
           #+allegro (excl:run-shell-command
                      (format nil "pdflatex ~a.tex" bare-name))
           #+sbcl (sb-ext:run-program "pdflatex" (list (format nil "~a.tex"
                                                         bare-name))
                                      :wait t :search t :output nil :error t)
;;;           #+clozure (run-program "pdflatex" (list (format nil "~a.tex"
;;;                                                     bare-name))
;;;                                  :wait t :search t :output nil :error t)
;;;           #+clisp (ext:shell (format nil "pdflatex ~a.tex" bare-name))
;;;           #+lispworks (system:call-system
;;;                        (format nil "pdflatex ~a.tex" bare-name))
           #-(or allegro sbcl) (error "Not implemented on this system."))
         (run-bibtex ()
           #+allegro (excl:run-shell-command
                      (format nil "bibtex ~a" bare-name))
           #+sbcl (sb-ext:run-program "bibtex" (list bare-name)
                                      :wait t :search t :output nil :error t)
;;;           #+clozure (run-program "bibtex" (list bare-name)
;;;                                  :wait t :search t :output nil :error t)
;;;           #+clisp (ext:shell (format nil "bibtex ~a" bare-name))
;;;           #+lispworks (system:call-system
;;;                        (format nil "bibtex ~a" bare-name))
           #-(or allegro sbcl) (error "Not implemented on this system."))
         (run-makeindex ()
           #+allegro (excl:run-shell-command
                      (format nil "makeindex ~a" bare-name))
           #+sbcl (sb-ext:run-program "makeindex" (list bare-name)
                                      :wait t :search t :output nil :error t)
;;;           #+clozure (run-program "makeindex" (list bare-name)
;;;                                  :wait t :search t :output nil :error t)
;;;           #+clisp (ext:shell (format nil "makeindex ~a" bare-name))
;;;           #+lispworks (system:call-system
;;;                        (format nil "makeindex ~a" bare-name))
           #-(or allegro sbcl) (error "Not implemented on this system.")))

    (set-dir)
    (run-latex)
    (when bibtex (run-bibtex))
    (when index (run-makeindex))
    (when (or bibtex index) (run-latex))
    (run-latex)))
