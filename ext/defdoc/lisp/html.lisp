;;; File html.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2011 Smart Information Flow Technologies.
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
(in-package :defdoc-html)

(defmacro with-div-wrapper ((stream tag) &body body)
  `(progn
     (format ,stream "<~a>" ,tag)
     (pprint-newline :mandatory ,stream)
     (princ "  " ,stream)
     (pprint-logical-block (,stream '(1))
       ,@body)
     (pprint-newline :mandatory ,stream)
     (format ,stream "</~a>" ,tag)))

(defmacro with-span-wrapper ((stream tag) &body body)
  `(progn
     (format ,stream "<~a>" ,tag)
     (pprint-logical-block (,stream '(1)) ,@body)
     (format ,stream "</~a>" ,tag)))

;;; -----------------------------------------------------------------

(defclass html-style () ())

;;; -----------------------------------------------------------------

(defgeneric write-html-output-index-page (style output stream base-link
                                                &key &allow-other-keys)
  (:method ((style html-style) output out base-link
            &rest keyargs &key &allow-other-keys)
    (pprint-logical-block (out '(1))
      (with-div-wrapper (out 'html)
        (apply #'format-html-output-index-page-header-block
               style output out output base-link keyargs)
        (pprint-newline :mandatory out)
        (apply #'format-html-output-index-page-body
               style output out output base-link keyargs))
      (pprint-newline :mandatory out))))

(defgeneric format-html-output-index-page-header-block (style
                                                        output out top base-link
                                                        &key &allow-other-keys)
  (:method (style output out top base-link &key &allow-other-keys)
    (declare (ignore style output out top base-link)))
  (:method ((style html-style) output out top base-link
            &rest keyargs &key &allow-other-keys)
    (with-div-wrapper (out 'head)
      (apply #'format-html-output-index-page-headers
             style output out top base-link keyargs))))

(defgeneric format-html-output-index-page-body (style output out top base-link
                                                &key &allow-other-keys)
  (:method (style output out top base-link &key &allow-other-keys)
    (declare (ignore style output out top base-link)))
  (:method ((style html-style) output out top base-link &rest keyargs)
    (with-div-wrapper (out 'body)
      (apply #'format-doc out style output :base-link base-link :top top
             keyargs))))

(defgeneric format-output-html-pretitle (style stream output
                                               &key &allow-other-keys)
  (:method (style stream output &key base-link context &allow-other-keys)
    (declare (ignore output))
    (when context
      (princ "<hr />" stream)
      (loop for (enc . others) on (reverse context) do
        (format-link-to-content style stream enc base-link
               #'(lambda (o)
                   (let ((short (get-output-unit-short-title o)))
                     (cond
                       (short
                        (format-docspec-element style nil short stream))
                       (t (format-content-anchor stream style o base-link)))))
               )
        (when others (princ " - " stream)))
      (princ "<hr />" stream))))

(defmethod format-output-leader-material ((style html-style)
                                          stream output &rest keyargs)
  (apply #'format-output-html-pretitle style stream output keyargs)
  (call-next-method))

(defmethod format-output-leader-title ((style html-style) stream output
                                       &key top &allow-other-keys)
  (declare (special *output-nesting-depth*))
  (unless (or (> *output-nesting-depth* 1) (eq output top))
    (let ((top-title (get-output-unit-title top)))
      (with-div-wrapper (stream 'p)
        (cond
          (top-title (with-span-wrapper (stream 'b)
                       (format-docspec-element style nil top-title stream)))
          (t (with-span-wrapper (stream 'b)
               (format stream "~a" (type-of top)))))))
    (pprint-newline :mandatory stream))
  (with-span-wrapper (stream (format nil "h~d" (+ 1 *output-nesting-depth*)))
    (let ((this-title (get-output-unit-title output)))
      (cond
        (this-title   (format-docspec-element style nil this-title stream))
        (t   (format stream "~a" (type-of output))))))
  (pprint-newline :mandatory stream))

(defgeneric format-html-output-index-page-headers (style output out top
                                                   base-link
                                                   &key &allow-other-keys)
  (:method (style output out top base-link &key &allow-other-keys)
    (declare (ignore style output out top base-link)))
  (:method ((style html-style) output out top base-link
            &rest keyargs &key &allow-other-keys)
    (declare (ignore base-link keyargs))

    (with-span-wrapper (out 'title)
      (let ((this-title (get-output-unit-title output))
            (top-title (get-output-unit-title top)))
        (cond
          ((eq output top)
           (cond
             (this-title (format-docspec-element style nil this-title out))
             (t (format out "~a" (type-of output)))))
          (t (if top-title
                 (format-docspec-element style nil top-title out)
               (format out "~a" (type-of top)))
             (princ " - " out)
             (cond
               (this-title
                (format-docspec-element style nil this-title out))
               (t (format out "~a" (type-of output))))))))))

;;; -----------------------------------------------------------------

(defvar *output-unit-name-hash* nil)
(defvar *processed-output-units* nil)

(defmethod write-output ((style html-style) output-name directory file-root
                         &rest keyargs)
  (let* ((pathname (prepare-html-output-directory directory file-root))
         (*output-unit-name-hash* (make-hash-table :test 'eq))
         (*processed-output-units* (make-hash-table :test 'eq)))
    (setf (gethash output-name *output-unit-name-hash*)
          (format nil "index~a"
            (apply #'get-filename-extension
                   style output-name directory "index" keyargs)))
    (apply #'traverse-and-write-output-pages
           style output-name pathname keyargs)))

(defmethod get-filename-extension ((style html-style)
                                   output-name directory file-name-root
                                   &key &allow-other-keys)
  (declare (ignore output-name directory file-name-root))
  "")

(defgeneric traverse-and-write-output-pages (style output pathname
                                                   &key &allow-other-keys)

  ;; First method: when the output is a symbol, it's the name of an
  ;; output-contents instance.
  (:method ((style html-style) (this-name symbol) pathname &rest keyargs)
    (apply #'traverse-and-write-output-pages
           style (make-instance this-name) pathname keyargs))

  ;; Next: an actual output-contents instance.
  (:method ((style html-style) (output output-contents) pathname
            &rest keyargs &key (context nil) &allow-other-keys)
    ;; (format t "~%~%>> ~s~%" output)
    (let ((other-keyargs (prune-keyargs keyargs :context))
          (this-name (get-html-disambiguator output)))
      (unless (gethash this-name *processed-output-units*)
        (let ((this-file (gethash this-name *output-unit-name-hash*)))
          (unless this-file
            (setf this-file (get-content-link-filename output)
                  (gethash this-name *output-unit-name-hash*) this-file))

          ;; Write this output's HTML page
          (with-open-file (out (merge-pathnames this-file pathname)
                           :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
            (apply #'write-html-output-index-page
                   style output out #p"./" keyargs))
          (setf (gethash this-name *processed-output-units*) t)

          ;; Traverse this page's contents, and write their pages.
          (loop for item in (output-contents-contents output) do
            (apply #'traverse-and-write-output-pages
                   style item pathname
                   :context (cons output context) other-keyargs))))))

  ;; Next: a docspec.
  (:method ((style html-style) (spec standard-doc-spec) pathname
            &rest keyargs &key &allow-other-keys)
    (let ((this-file (get-content-link-filename spec)))

      ;; Write this output's HTML page
      (with-open-file (out (merge-pathnames this-file pathname)
                           :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
        (let ((*output-nesting-depth* 0))
          (apply #'format-doc out style spec keyargs)))))

  ;; Default: do nothing.
  (:method ((style html-style) (spec explicit-doc-element)
            pathname &key &allow-other-keys)
    (declare (ignore pathname)))
  (:method ((style html-style) (spec aftermatter-contents)
            pathname &key &allow-other-keys)
    (declare (ignore pathname)))
  )

(defgeneric get-html-disambiguator (item)
  (:method (item) (type-of item))
  (:method ((gr grouped-output-contents))
    (let ((title (get-output-unit-title gr)))
      (cond
        (title
         (intern
          (with-output-to-string (s)
            (format-docspec-element (make-instance 'standard-docstring-style)
                                    nil title s))))
        (t (gensym))))))

(defun prepare-html-output-directory (directory file-name)
  (let ((path-to-file (merge-pathnames file-name directory))
        (path-to-dir (merge-pathnames (concatenate 'string file-name "/")
                                      directory)))
    (when (and path-to-file (not path-to-dir))
      (delete-file path-to-file))
    (ensure-directories-exist path-to-dir)
    path-to-dir))

;;; -----------------------------------------------------------------

(defun format-link-to-content (style stream output base-link
                                     &optional (anchor-extractor nil))
  (unless anchor-extractor
    (setf anchor-extractor
          #'(lambda (o) (format-content-anchor stream style o base-link))))
  (format stream "<a href=\"")
  (format-content-link stream style output base-link)
  (format stream "\">")
  (funcall anchor-extractor output)
  (format stream "</a>"))


(defmethod format-doc (stream (style html-style) output
                              &key base-link &allow-other-keys)
  (declare (special *output-nesting-depth*))
  (cond
    ((> *output-nesting-depth* 0)
     (format-link-to-content style stream output base-link))
    (t (call-next-method))))

(defgeneric format-content-link (stream style output base-link)
  (:method (stream (style html-style) output base-link)
    (let ((fname (get-content-link-filename output)))
      (when fname
        (format stream "~a" (merge-pathnames fname base-link))))))

(defgeneric get-content-link-filename (item)
  (:method ((spec standard-doc-spec))
    (format nil "~a__~a.html" (docspec-self spec) (docspec-target-type spec)))
  (:method ((output output-contents))
    (let ((title (get-output-unit-title output)))
      (cond
        (title (with-output-to-string (s)
                 (princ "__" s)
                 (format-docspec-element (make-instance
                                             'standard-docstring-style)
                                         nil title s)
                 (princ ".html" s)))
        (t (concatenate 'string (symbol-name (gensym)) ".html")))))
  (:method ((output grouped-output-contents))
    (let ((title (get-output-unit-title output)))
      (with-output-to-string (s)
        (cond
          (title (princ "__" s)
                 (format-docspec-element (make-instance
                                             'standard-docstring-style)
                                         nil title s)
                 (princ ".html" s))
          (t (format s "___~a__~a.html"
               (doc-label-name (get-grouped-output-labeldef output))
               (get-grouped-output-group output)))))))
  (:method (misc)
    (declare (ignore misc))
    nil))

(defgeneric format-content-anchor (stream style output base-link)
  (:method (stream (style html-style) output base-link)
    (declare (ignore stream output base-link)))
  (:method (stream (style html-style) (output output-contents) base-link)
    (declare (ignore base-link))
    (let ((title (get-output-unit-title output)))
      (cond
        (title (format-docspec-element style nil title stream))
        (t (princ "Untitled" stream)))))
  (:method (stream (style html-style) (spec standard-doc-spec) base-link)
    (declare (ignore base-link))
    (format stream "~a <tt>~a</tt>"
      (capitalized-target-name (get-target-type (docspec-target-type spec)))
      (docspec-self spec))))

(defmethod format-output-leader-title :after ((style html-style) stream output
                                              &key &allow-other-keys)
  (declare (ignore output))
  (pprint-newline :mandatory stream))

(defmethod format-doc-content-items :around (stream (style html-style) output
                                                    &key &allow-other-keys)
  (declare (ignore output))
  (with-div-wrapper (stream 'ul)
    (call-next-method)))

(defmethod format-doc-content-item :around (stream (style html-style) output
                                                   &key &allow-other-keys)
  (declare (ignore output))
  (with-div-wrapper (stream 'li)
    (call-next-method)))

(defmethod format-output-contents-sep :after ((style html-style) stream out i j
                                              &key &allow-other-keys)
  (declare (ignore out i j))
  (pprint-newline :mandatory stream))

(defmethod format-output-leader-docspec :around ((style html-style)
                                                 leader stream)
  (cond
    ((html-free-paragraph-docspec-element leader)
     (call-next-method))
    (t (with-div-wrapper (stream 'p) (call-next-method)))))

(defmethod format-output-leader-sep ((style html-style) stream output)
  (declare (ignore output))
  (pprint-newline :mandatory stream))

(defmethod format-output-trailer-docspec :around ((style html-style)
                                                  trailer stream)
  (cond
    ((html-free-paragraph-docspec-element trailer)
     (call-next-method))
    (t (with-div-wrapper (stream 'p) (call-next-method)))))

(defgeneric html-free-paragraph-docspec-element (e)
  (:method (e) (declare (ignore e)) nil)
  (:method ((e standard-paragraph-list)) t)
  (:method ((e standard-itemize)) t)
  (:method ((e standard-enumerate)) t)
  (:method ((e standard-code)) t))

;;; -----------------------------------------------------------------

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-plain-text) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "~a" (text-element-text spec)))

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-paragraph-list) stream
                                   &key &allow-other-keys)
  (loop for (par . others) on (paragraphlist-element-items spec) do
    (with-div-wrapper (stream 'p)
      (format-docspec-element style target-type par stream))
    (when others (pprint-newline :mandatory stream))))

(defmethod format-docspec-element ((style html-style) target-type
                                   (code standard-code) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (with-span-wrapper (stream 'pre)
    (princ (code-element-string code) stream)))

(defmethod format-docspec-element ((style html-style) target-type
                                   (code standard-inline) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (with-span-wrapper (stream 'tt)
    (princ (inline-element-string code) stream)))

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-itemize) stream
                                   &key &allow-other-keys)
  (with-div-wrapper (stream 'ul)
    (loop for (item . others) on (list-element-specs spec) do
      (with-div-wrapper (stream 'li)
        (format-docspec-element style target-type item stream))
      (when others (pprint-newline :mandatory stream)))))

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-enumerate) stream
                                   &key &allow-other-keys)
  (with-div-wrapper (stream 'ol)
    (loop for (item . others) on (list-element-specs spec) do
      (with-div-wrapper (stream 'li)
        (format-docspec-element style target-type item stream))
      (when others (pprint-newline :mandatory stream)))))

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-lisp-name) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "<tt>~a</tt>" (lisp-name spec)))

(defmethod format-docspec-element ((style html-style) target-type
                                   (spec standard-emphasized) stream
                                   &rest keyvals)
  (with-span-wrapper (stream 'em)
    (apply #'format-docspec-element
           style target-type (emphasized-spec spec) stream keyvals)))

(defmethod format-docspec-element ((style html-style) type
                                   (spec standard-latex) stream
                                   &key &allow-other-keys)
  (format-docspec-element style type (canonicalize-element spec) stream))

(defmethod format-docspec-element ((style html-style) target-type
                                   (name latex-name-element) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "L<small><sup>A</sup></small>T<sub>E</sub>X"))

(defmethod format-docspec-element ((style html-style) target-type
                                   (name bibtex-name-element) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "BibT<sub>E</sub>X"))

(defmethod format-docspec-element ((style html-style) target-type
                                   (name standard-fillin-place) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type))
  (format stream "<font color=\"#ff0000\"><b>FILL IN</b></font>"))

(defmethod format-docspec-element ((style html-style) target-type
                                   (doc standard-outputset-element) stream
                                   &rest keyvals)
  (declare (ignore target-type))
  (apply #'format-output-contents-actual
         stream style (make-instance (output-elem-name doc)) keyvals))

(defmethod format-docspec-element ((style html-style) target-type
                                   (doc standard-reference) stream
                                   &key &allow-other-keys)
  (declare (ignore target-type stream))
  (warn "Generating nothing for reference ~s" doc))

;;; -----------------------------------------------------------------

(defmethod format-standard-docspec-literal-text ((style html-style)
                                                 text stream
                                                 &key &allow-other-keys)
  (format stream "<pre>~a</pre>" text))

(defmethod format-standard-docspec-param-list
    :around ((style html-style) type spec stream &key &allow-other-keys)
  (declare (ignore spec type))
  (with-div-wrapper (stream 'dl)
    (call-next-method)))

(defmethod format-standard-docspec-param-list-item
    :around ((style html-style) type spec name subspec stream
             &key &allow-other-keys)
  (declare (ignore spec subspec type))
  (with-span-wrapper (stream 'dt)
    (princ name stream))
  (pprint-newline :mandatory stream)
  (with-div-wrapper (stream 'dd)
    (call-next-method)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Methods for HTML styles that uses an itemized list for organizing
;;; the specs.

(defmethod format-itemized-list-start ((style html-style) stream)
  (format stream "<ul>"))
(defmethod format-itemized-list-end ((style html-style) stream)
  (format stream "</ul>"))
(defmethod format-itemized-list-item-start ((style html-style) stream)
  (format stream "<li>"))
(defmethod format-itemized-list-item-end ((style html-style) stream)
  (format stream "</li>"))




