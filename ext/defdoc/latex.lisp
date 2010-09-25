
(in-package :defdoc)

;;; -----------------------------------------------------------------
;;; Top-level LaTeX generation APIs.

(defun write-spec-latex (name usage &key
                              (style 'latex-style)
                              (directory *defdoc-latex-default-directory*)
                              (file (concatenate 'string
                                      (symbol-name name) "_"
                                      (symbol-name usage)"_"
                                      (symbol-name style) ".tex")))
  ;; (format t "----------~%style ~s~%dir ~s~%file ~s~%" style directory file)
  #+sbcl
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
      (format-doc out style (get-doc-spec name usage)))))

(defun write-doctype-latex (doctype &key (echo nil echo-supp)
                                    (directory *defdoc-latex-default-directory*)
                                    (style 'latex-style))
  (loop for name being the hash-keys of (gethash doctype +defdocs+) do
    (when echo-supp
      (funcall echo :name name :type doctype))
    (write-spec-latex name doctype :directory directory :style style)))

(defun write-package-specs-latex (package-specifier
                                  &key (echo nil echo-supp)
                                  (directory *defdoc-latex-default-directory*)
                                  (style 'latex-style)
                                  (package-style 'full-package-latex-style))
  (let ((package (find-package package-specifier)))
    (do-external-symbols (sym package)
      (loop for form in (get-doctypes) do
        (when (get-doc-spec sym form)
          (when echo-supp
            (funcall echo :name sym :type form))
          (write-spec-latex sym form :directory directory :style style))))
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

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :spec)) stream spec-args)
  (destructuring-bind (&key self
                            (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p)
                            (callspec nil callspec-supp-p)
                       &allow-other-keys) spec-args
    (cond
     (intro-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style spec-type in
                                                       :intro spec-args intro)
                      spec-type))
     ((and short-supp-p (not full-supp-p))
      (format-docspec stream style
                      (latex-style-adjust-spec-element style spec-type in
                                                       :short spec-args short)
                      spec-type)))

    (when callspec-supp-p
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
        (format-docspec stream style
                        (latex-style-adjust-spec-element style spec-type in
                              :subspec spec-args subspec)
                        spec-type))
      (princ "\\end{description}" stream))

    (when full-supp-p
      (format-docspec stream style
                      (latex-style-adjust-spec-element style spec-type in
                                                       :full spec-args full)
                      spec-type))))

(defgeneric latex-style-adjust-spec-element (style spec-type spec-head element
                                                   spec-args datum)
  (:method (style spec-type spec-head element
                  spec-args datum)
     (declare (ignore style spec-type spec-head element spec-args))
     datum))

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :plain)) stream args)
  (declare (ignore spec-type))
  (destructuring-bind (string) args
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

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :latex)) stream spec-args)
  (declare (ignore spec-type))
  (princ (car spec-args) stream))

(defmethod format-docspec-element ((style latex-style) spec-type (in (eql :seq))
                                   stream spec-args)
  (loop for (p-spec . other-specs) on spec-args do
    (format-docspec stream style p-spec spec-type)
    (when other-specs (princ " " stream))))


(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :code)) stream args)
  (declare (ignore spec-type))
  (destructuring-bind (string) args
    (format stream "\\begin{verbatim}~a\\end{verbatim}" string)))

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :paragraphs)) stream args)
  (loop for (p-spec . other-specs) on args do
    (format-docspec stream style p-spec spec-type)
    (when other-specs (princ "\\par " stream))))

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :enumerate)) stream args)
  (let ((options (pop args)))
    (declare (ignore options))
    (format-latex-list stream style spec-type "enumerate" args)))

(defmethod format-docspec-element ((style latex-style) spec-type
                                   (in (eql :itemize)) stream args)
  (let ((options (pop args)))
    (declare (ignore options))
    (format-latex-list stream style spec-type "itemize" args)))

(defun format-latex-list (stream style spec-type list-tag specs)
  (format stream "\\begin{~a}" list-tag)
  (loop for spec in specs do
    (princ "\\item " stream)
    (format-docspec stream style spec spec-type))
  (format stream "\\end{~a}" list-tag))

;;; -----------------------------------------------------------------
;;; Style for a full description of packages.

(defclass full-package-latex-style (latex-style) ())

(defmethod format-docspec-element ((style full-package-latex-style)
                                   (spec-type (eql 'package)) (in (eql :spec))
                                   stream spec-args)
  (destructuring-bind (&key self &allow-other-keys) spec-args
    (call-next-method)
    (do-external-symbols (var self)
      (format stream "~a{~a}" *latex-full-package-item-header-macro* var)
      (loop for var-type in (get-symbol-doctypes var) do
        (format-docspec stream style
                               (get-doc-spec var var-type) spec-type)))))

;;; -----------------------------------------------------------------
;;; Style for a sectioned listing of the symbols exported in a
;;; package.

(defclass package-list-latex-style (latex-style) ())

(defgeneric package-list-overall-header (style spec stream)
  (:method ((style package-list-latex-style) spec stream)
     (destructuring-bind (&key descriptive &allow-other-keys) (cdr spec)
       (format stream "\\section{The ~a API}~%" descriptive))))
(defgeneric package-list-group-header (style spec group stream)
  (:method ((style package-list-latex-style) spec group stream)
     (princ "\\subsection{" stream)
     (destructuring-bind (&key self &allow-other-keys) (cdr spec)
       ;; (format t "~s ~s~%" self group)
       (format-tag style (find-package self) group stream))
     (format stream "}~%")))
(defgeneric package-list-entry (style spec group entry stream)
  (:method ((style package-list-latex-style) spec group entry stream)
     (declare (ignore spec group))
     (destructuring-bind (&key self &allow-other-keys) (cdr entry)
       (format stream "\\texttt{~a}~%~%" self))))

(defmethod format-docspec-element ((style package-list-latex-style)
                                   (spec-type (eql 'package)) (in (eql :spec))
                                   stream spec-args)
  (destructuring-bind (&key self &allow-other-keys) spec-args
    (let ((actual-package (find-package self))
          (tag-sort (make-hash-table :test 'eq))
          (pspec (get-doc-spec self spec-type)))
      (do-external-symbols (sym self)
        (loop for type in (get-symbol-doctypes sym) do
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
