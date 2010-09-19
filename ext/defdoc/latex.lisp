
(in-package :defdoc)

(defvar *latex-verbatim-width* 65)
(defclass latex-style () ())

(defmethod format-docspec-element ((in (eql :spec)) stream (style latex-style) spec-args)
  (destructuring-bind (&key self
                            (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p)
                            (callspec nil callspec-supp-p)
                            &allow-other-keys) spec-args
    (flet ((generate-intro ()
             (when intro-supp-p (format-docspec stream style intro)))
           (generate-full ()
             (when full-supp-p (format-docspec stream style full)))
           (generate-short ()
             (when short-supp-p (format-docspec stream style short)))
           (generate-params ()
             (when params-supp-p
               (princ "\\begin{description}" stream)
               (loop for (name subspec) in params do
                 (format stream "\\item[~a] " name)
                 (format-docspec stream style subspec))
               (princ "\\end{description}" stream)))
           (generate-callspec ()
             (when callspec-supp-p
               (princ " \\begin{verbatim}" stream)
               (loop for (cspec . others) on callspec do
                 (loop for line
                       in (callspec-to-lines cspec *latex-verbatim-width* self)
                       do (format stream "  ~a~%" line))
                 (when others (format stream "~%")))
               (princ "\\end{verbatim}" stream))))
      (cond
        ((or intro-supp-p full-supp-p)
         (generate-intro)
         (generate-callspec)
         (generate-params)
         (generate-full))

        (short-supp-p
         (generate-short)
         (generate-callspec)
         (generate-params))

        (params-supp-p
         (generate-callspec)
         (generate-params))

        (t nil)))))

(defmethod format-docspec-element ((in (eql :plain)) stream
                                   (style latex-style) args)
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

(defmethod format-docspec-element ((in (eql :latex)) stream
                                   (style latex-style) spec-args)
  (princ (car spec-args) stream))

(defmethod format-docspec-element ((in (eql :seq)) stream
                                   (style latex-style) spec-args)
  (loop for (p-spec . other-specs) on spec-args do
    (format-docspec stream style p-spec)
    (when other-specs (princ " " stream))))


(defmethod format-docspec-element ((in (eql :code)) stream
                                   (style latex-style) args)
  (destructuring-bind (string) args
    (format stream "\\begin{verbatim}~a\\end{verbatim}" string)))

(defmethod format-docspec-element ((in (eql :paragraphs)) stream
                                   (style latex-style) args)
  (loop for (p-spec . other-specs) on args do
    (format-docspec stream style p-spec)
    (when other-specs (princ "\\par " stream))))

(defmethod format-docspec-element ((in (eql :enumerate)) stream
                                   (style latex-style) args)
  (let ((options (pop args)))
    (declare (ignore options))
    (format-latex-list stream style "enumerate" args)))

(defmethod format-docspec-element ((in (eql :itemize)) stream
                                   (style latex-style) args)
  (let ((options (pop args)))
    (declare (ignore options))
    (format-latex-list stream style "itemize" args)))

(defun format-latex-list (stream style list-tag specs)
  (format stream "\\begin{~a}" list-tag)
  (loop for spec in specs do
    (princ "\\item " stream)
    (format-docspec stream style spec))
  (format stream "\\end{~a}" list-tag))

(defvar *defdoc-latex-default-directory* #p"./")
(defun write-spec-latex (name usage &key
                              (style 'latex-style)
                              (directory *defdoc-latex-default-directory*)
                              (file (concatenate 'string
                                      (symbol-name name) "_"
                                      (symbol-name usage) ".tex")))
  (let ((file-spec (merge-pathnames file directory)))
    (with-open-file (out file-spec
                     :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
      (format-docspec out style (get-doc-spec name usage)))))

(defun write-package-specs-latex (package-specifier
                                  &optional (echo nil echo-supp)
                                  (directory *defdoc-latex-default-directory*)
                                  (style 'latex-style))
  (let ((package (find-package package-specifier)))
    (do-external-symbols (sym package)
      (loop for form in (get-doctypes) do
        (when (get-doc-spec sym form)
          (when echo-supp
            (funcall echo :name sym :type form))
          (write-spec-latex sym form :directory directory :style style))))))
