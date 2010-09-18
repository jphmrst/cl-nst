
(in-package :defdoc)

(defun spec-to-latex (spec)
  (output-latex (car spec) (cdr spec)))

(defgeneric output-latex (spec-head spec-args))

(defvar *latex-verbatim-width* 65)

(defmethod output-latex ((in (eql :spec)) spec-args)
  (destructuring-bind (&key self
                            (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p)
                            (callspec nil callspec-supp-p)
                            &allow-other-keys) spec-args
    (apply #'concatenate 'string
      (cond
        ((or intro-supp-p full-supp-p)
         (append (when intro-supp-p
                   (list (spec-to-latex intro)))
                 (when callspec-supp-p
                   `(" \\begin{verbatim}"
                     ,@(loop for (cspec . others) on callspec
                             append
                             (append
                              (loop for line
                                    in (callspec-to-lines cspec
                                                *latex-verbatim-width* self)
                                  collect (concatenate 'string
                                            "  " line (format nil "~%")))
                              (when others (list ""))))
                     "\\end{verbatim}"))
                 (when params-supp-p
                   `("\\begin{description}"
                     ,@(loop for (name subspec) in params
                           append (list "\\item["
                                        (format nil "~a" name)
                                        "]"
                                        (spec-to-latex subspec)))
                     "\\end{description}"))
                 (when full-supp-p
                   (list (spec-to-latex full)))))

        (short-supp-p
         (append (list (spec-to-latex short))
                 (when callspec-supp-p
                   `(" \\begin{verbatim}"
                     ,@(loop for cspec in callspec
                           append (loop for line
                                        in (callspec-to-lines cspec
                                                    *latex-verbatim-width* self)
                                        collect (concatenate 'string
                                                  "  " line (format nil "~%"))))
                     "\\end{verbatim}"))
                 (when params-supp-p
                   (loop for (name subspec) in params
                       append (list "\\item["
                                    (format nil "~a" name)
                                    "]"
                                    (spec-to-latex subspec))))))

        (params-supp-p
         (append (when callspec-supp-p
                   `(" \\begin{verbatim}"
                     ,@(loop for cspec in callspec
                           append (loop for line
                                        in (callspec-to-lines cspec
                                                    *latex-verbatim-width* self)
                                        collect (concatenate 'string
                                                  "  " line (format nil "~%"))))
                     "\\end{verbatim}"))
                 (when params-supp-p
                   (loop for (name subspec) in params
                       append (list "\\item["
                                    (format nil "~a" name)
                                    "]"
                                    (spec-to-latex subspec))))))

        (t nil)))))

(defmethod output-latex ((in (eql :plain)) args)
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
          finally (return (coerce characters 'string))))))

(defmethod output-latex ((in (eql :latex)) spec-args)
  (car spec-args))

(defmethod output-latex ((in (eql :seq)) spec-args)
  (apply #'concatenate 'string
         (loop for (p-spec . other-specs) on spec-args
               append (let ((par (spec-to-latex p-spec)))
                        `(,par ,@(when other-specs (list " ")))))))


(defmethod output-latex ((in (eql :code)) spec-args)
  (destructuring-bind (string) spec-args
    (concatenate 'string "\\begin{verbatim}" string "\\end{verbatim}")))

(defmethod output-latex ((in (eql :paragraphs)) spec-args)
  (apply #'concatenate 'string
         (loop for (p-spec . other-specs) on spec-args
               append (let ((par (spec-to-latex p-spec)))
                        `(,par ,@(when other-specs (list "\\par ")))))))

(defmethod output-latex ((in (eql :enumerate)) args)
  (let ((options (pop args)))
    (declare (ignore options))
    (build-latex-list "enumerate" args)))

(defmethod output-latex ((in (eql :itemize)) args)
  (let ((options (pop args)))
    (declare (ignore options))
    (build-latex-list "itemize" args)))

(defun build-latex-list (list-tag specs)
  (apply #'concatenate 'string
         (nconc (list "\\begin{" list-tag "}")
                (loop for spec in specs
                      append (list "\\item " (spec-to-latex spec)))
                (list "\\end{" list-tag "}"))))

(defun get-spec-latex (name usage)
  (spec-to-latex (get-doc-spec name usage)))

(defvar *defdoc-latex-default-directory* #p"./")
(defun write-spec-latex (name usage &key
                              (directory *defdoc-latex-default-directory*)
                              (file (concatenate 'string
                                      (symbol-name name) "_"
                                      (symbol-name usage) ".tex")))
  (let ((file-spec (merge-pathnames file directory)))
    (with-open-file (out file-spec
                     :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
      (format out "~a~%" (get-spec-latex name usage)))))

(defun write-package-specs-latex (package-specifier
                                  &optional (echo nil echo-supp)
                                  (directory *defdoc-latex-default-directory*))
  (let ((package (find-package package-specifier)))
    (do-external-symbols (sym package)
      (loop for form in (get-doctypes) do
        (when (get-doc-spec sym form)
          (when echo-supp
            (funcall echo :name sym :type form))
          (write-spec-latex sym form :directory directory))))))
