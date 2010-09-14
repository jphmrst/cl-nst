
(in-package :defdoc)

(defun spec-to-latex (spec)
  (output-latex (car spec) (cdr spec)))

(defgeneric output-latex (spec-head spec-args))

(defmethod output-latex ((in (eql :spec)) spec-args)
  (destructuring-bind (&key (intro nil intro-supp-p)
                            (params nil params-supp-p)
                            (short nil short-supp-p)
                            (full nil full-supp-p) &allow-other-keys) spec-args
    (apply #'concatenate 'string
      (cond
        ((or intro-supp-p full-supp-p)
           (nconc (when intro-supp-p
                    (list (spec-to-latex intro)))
                  (when params-supp-p
                    (nconc '("\\begin{description}")
                           (loop for (name subspec) in params
                               append (list "\\item["
                                            (format nil "~a" name)
                                            "]"
                                            (spec-to-latex subspec)))
                           '("\\end{description}")))
                  (when full-supp-p
                    (list (spec-to-latex full)))))

        (short-supp-p
         (list* (spec-to-text short)
                (when params-supp-p
                  (loop for (name subspec) in params
                        append (list "\\item["
                                        (format nil "~a" name)
                                        "]"
                                        (spec-to-latex subspec))))))

        (params-supp-p
         (when params-supp-p
           (loop for (name subspec) in params
               append (list "\\item["
                            (format nil "~a" name)
                            "]"
                            (spec-to-latex subspec)))))

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

(defmethod output-latex ((in (eql :paragraphs)) spec-args)
  (apply #'concatenate 'string
         (loop for (p-spec . other-specs) on spec-args
               append (let ((par (spec-to-latex p-spec)))
                        `(,par ,@(when other-specs (list "\\par")))))))

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
         (nconc "\\begin{" list-tag "}"
                (loop for spec in specs
                      append (list "\\item " (spec-to-latex spec)))
                (list "\\end{" list-tag "}"))))

(defun get-spec-latex (name usage)
  (spec-to-latex (gethash name (gethash usage +defdocs+))))

(defun write-spec-latex (name usage &key (directory #p"./") (file "out.tex"))
  (with-open-file (out (merge-pathnames file directory)
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~a~%"
      (spec-to-latex (gethash name (gethash usage +defdocs+))))))
