;;; File system.lisp
;;;
;;; This file is part of the Defdoc unit/regression testing system.
;;;
;;; Copyright (c) 2010, 2011 Smart Information Flow Technologies.
;;;
;;; Defdoc is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Defdoc is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Defdoc.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :sift.asdf-defdoc)


(defclass defdoc-asdf (system)
    ((documents-system :initarg :documents-system
                       :reader documents-system
                       :initform nil
                       :documentation "Output documents to be generated.")
     (documentation-package :initarg :documentation-package
                            :reader documentation-package
                            :initform *package*
                            :documentation
                            "Default package for symbol references.")
     (build-output :initarg :build-output
                   :reader build-output
                   :initform nil
                   :documentation "Output documents to be generated."))

  (:documentation
   "Class of ASDF systems that generate documentation when loaded."))

(defmethod asdf::component-depends-on :around ((op compile-op)
                                               (sys defdoc-asdf))
  `((asdf:load-op :defdoc)
    ,@(let ((base (documents-system sys)))
        (when base
          `((asdf:load-op ,(documents-system sys)))))
    ,@(call-next-method)))

(defmethod asdf::component-depends-on :around ((op load-op) (sys defdoc-asdf))
  `((asdf:load-op :defdoc)
    ,@(let ((base (documents-system sys)))
        (when base
          `((asdf:load-op ,(documents-system sys)))))
    ,@(call-next-method)))

(defmethod perform ((o asdf:load-op) (c defdoc-asdf))

  ;; First, do whatever ASDF usually does for loading this system.
  (call-next-method)

  (let ((outputs (build-output c)))
    (when (symbolp outputs)
      (setf outputs (list outputs)))

    (loop for output in outputs do
      (let (output-name dir-pathname file-name idx toc doc-style)
        (cond
         ;; If the output spec is just the symbolic name, make the
         ;; obvious guesses for the directory and filename.
         ((symbolp output)
          (setf output-name (intern (symbol-name output)
                                    (find-package (documentation-package c)))
                dir-pathname (asdf:system-relative-pathname c "./")
                file-name (symbol-name output)
                idx nil
                toc nil
                doc-style (intern :#latex-style (find-package :defdoc))))

         ((listp output)
          (destructuring-bind (name &key
                                    (package (documentation-package c))
                                    (rel-directory nil rel-directory-supp-p)
                                    (abs-directory nil abs-directory-supp-p)
                                    (filename-root nil filename-root-supp-p)
                                    index
                                    table-of-contents
                                    (style nil style-supp-p)) output

            (when (and rel-directory-supp-p abs-directory-supp-p)
              (error
               "Provide either :rel-directory or :abs-directory but not both"))

            (setf output-name (intern (symbol-name name) (find-package package))

                  dir-pathname
                  (cond
                   (rel-directory-supp-p
                    (asdf:system-relative-pathname c rel-directory))
                   (abs-directory-supp-p abs-directory)
                   (t (asdf:system-relative-pathname c "./")))

                  file-name
                  (cond
                   (filename-root-supp-p filename-root)
                   (t (symbol-name name)))

                  idx index
                  toc table-of-contents

                  doc-style (cond
                             (style-supp-p (intern (symbol-name style)
                                                   (find-package package)))
                             (t (intern :#latex-style
                                        (find-package :defdoc)))))))

         (t (error "Expected symbol or list, got ~s" output)))

        (funcall (symbol-function (intern (symbol-name '#:write-latex-output)
                                          (find-package :defdoc)))
                 output-name
                 :echo #'(lambda (&key &allow-other-keys)
                           (format t "Writing ~a~%" output-name))
                 :directory dir-pathname
                 :file (format nil "~a.tex" file-name)
                 :standalone t
                 :index idx :table-of-contents toc
                 :style doc-style)
        (funcall (symbol-function (intern (symbol-name
                                           '#:process-latex-document)
                                          (find-package :defdoc)))
                 dir-pathname file-name :index idx)))))

(defmethod operation-done-p ((op load-op) (c defdoc-asdf))
  nil)
