
(in-package :defdoc)

;;; -----------------------------------------------------------------
;;; Top-level LaTeX generation APIs.

(defvar *defdoc-latex-default-directory* "./")
(defvar *latex-full-package-item-header-macro* "\\paragraph")

(defgeneric get-latex-output-file-name (style usage name)
  (:method ((style symbol) usage name)
           (concatenate 'string
             (symbol-name name) "_"
             (symbol-name usage) "_"
             (symbol-name style) ".tex"))
  (:method (style usage name)
     (get-latex-output-file-name (type-of style) usage name)))

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
  (loop for name being the hash-keys of (gethash doctype +defdocs+) do
    (when echo-supp
      (funcall echo :name name :type doctype))
    (write-spec-latex name doctype :directory dir :style style)))

(defun write-package-specs-latex (package-specifier
                                  &key (echo nil echo-supp)
                                  (directory *defdoc-latex-default-directory*)
                                  (style 'latex-style)
                                  (package-style 'full-package-latex-style)
                                  include-doctypes)
  (let ((package (find-package package-specifier)))
    (do-external-symbols (sym package)
      (loop for form in (get-doc-target-types) do
        (when (get-doc-spec sym form)
          (when echo-supp
            (funcall echo :name sym :type form))
          (write-spec-latex sym form :directory directory :style style))))

    (loop for doctype in include-doctypes do
      (loop for sym being the hash-keys of (gethash doctype +defdocs+) do
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

(defmethod format-docspec-element ((style latex-style) target-type
                                   (spec standard-doc-spec) stream)
  (with-unpacked-standard-spec (self intro intro-supp-p params params-supp-p
                                     short short-supp-p full full-supp-p
                                     callspec) spec
    (cond
     (intro-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style target-type spec
                                                       :intro intro)
                      target-type))
     ((and short-supp-p (not full-supp-p))
      (format-docspec stream style
                      (latex-style-adjust-spec-element style target-type spec
                                                       :short short)
                      target-type)))

    (when callspec
      (princ " \\begin{verbatim}" stream)
      (loop for (cs . others) on callspec do
        (loop for line in (callspec-to-lines cs *latex-verbatim-width* self) do
          (format stream "  ~a~%" line))
        (when others (format stream "~%")))
      (princ "\\end{verbatim}" stream))

    (when params-supp-p
      (princ "\\begin{description}" stream)
      (loop for (name subspec) in params do
        (format stream "\\item[~a] " name)
        (format-docspec stream
               style (latex-style-adjust-spec-element style target-type
                           spec :params subspec)
                        target-type))
      (princ "\\end{description}" stream))

    (when full-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style target-type spec
                                                       :full full)
                      target-type))))

(defgeneric latex-style-adjust-spec-element (style target-type spec
                                                   element datum)
  (:method (style target-type spec element datum)
     (declare (ignore style target-type spec element))
     datum))

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

;;; -----------------------------------------------------------------
;;; Mixin for a full description of packages.

(defclass full-package-latex-style-mixin () ())

(defmethod format-docspec-element ((style full-package-latex-style-mixin)
                                   (target-type (eql 'package))
                                   (spec standard-doc-spec) stream)
  (with-accessors ((docspec-self self)) spec
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

(defmethod format-docspec-element ((style package-list-latex-mixin)
                                   (target-type (eql 'package))
                                   (spec standard-doc-spec) stream)
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
              in (sort tag-list #'(lambda (x y)
                                    (let ((sx (tag-sort style actual-package x))
                                          (sy (tag-sort style
                                                        actual-package y)))
                                      (< sx sy))))
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
