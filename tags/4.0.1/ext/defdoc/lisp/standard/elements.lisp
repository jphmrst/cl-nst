;;; File elements.lisp
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

(in-package :defdoc-standard-model)

(defclass standard-doc-element (docspec-element)
     ())

(def-element :plain (standard-plain-text :class standard-doc-element
                                         :args (text))
    ((text :initarg :text :reader text-element-text))
  (make-instance 'standard-plain-text :text text))
(set-pprint-dispatch 'standard-plain-text
  (named-function pprint-standard-plain-text
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(text) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))
(defmethod spaceheaded-element ((element standard-plain-text))
  (whitespace-p (elt (text-element-text element) 0)))

(def-element :ref (standard-reference :class standard-doc-element
                                      :args (name))
    ((name :initarg :name :reader referenced-name))
  (make-instance 'standard-reference :name name))
(set-pprint-dispatch 'standard-reference
  (named-function pprint-standard-reference
    (lambda (stream spec)
      (format stream "{reference to ~s}" (referenced-name spec)))))

(def-element :lisp (standard-lisp-name :class standard-doc-element
                                       :args (kind name))
    ((kind :initarg :kind :reader lisp-name-kind)
     (name :initarg :name :reader lisp-name))
  (unless (get-target-type kind t)
    (warn "No target-type ~s, used as :lisp name kind of ~s" kind name))
  (make-instance 'standard-lisp-name :kind kind :name name))
(set-pprint-dispatch 'standard-lisp-name
  (named-function pprint-standard-lisp-name
    (lambda (stream spec)
      (format stream "{~a ~s}" (lisp-name-kind spec) (lisp-name spec)))))

(def-element :emph (standard-emphasized :class standard-doc-element
                                        :package package :spec spec
                                        :args (&rest specs))
    ((spec :initarg :spec :reader emphasized-spec))
  (make-instance 'standard-emphasized
    :spec (cond
            ((eql (length specs) 1) (compile-element package spec (car specs)))
            (t (mapcar (named-function make-standard-emph-mapper
                          (lambda (x) (compile-element package spec x)))
                       specs)))))
(set-pprint-dispatch 'standard-emphasized
  (named-function pprint-standard-emphasized
    (lambda (stream spec)
      (format stream "{emph: ~w}" (emphasized-spec spec)))))
(defmethod spaceheaded-element ((element standard-emphasized))
  (spaceheaded-element (emphasized-spec element)))

(def-element :paragraphs (standard-paragraph-list :class standard-doc-element
                                                  :package package :spec spec
                                                  :arg-list args)
    ((paragraphs :initarg :paragraphs :reader paragraphlist-element-items))
  (make-instance 'standard-paragraph-list
    :paragraphs (mapcar (named-function make-standard-paragraph-list-mapper
                          (lambda (x)
                            (compile-element package spec x)))
                        args)))
(set-pprint-dispatch 'standard-paragraph-list
  (named-function pprint-standard-paragraph-list
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(paragraphs) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :seq (standard-sequence :class standard-doc-element
                                     :package package :spec spec
                                     :arg-list args)
     ((elements :initarg :elements :reader sequence-element-items))
  (make-instance 'standard-sequence
    :elements (mapcar (named-function make-standard-sequence-mapper
                        (lambda (x)
                          (compile-element package spec x)))
                      args)))
(set-pprint-dispatch 'standard-sequence
  (named-function pprint-standard-sequence
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(elements) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))

(defmethod format-docspec-element (style target-type (spec standard-sequence)
                                         stream &rest keyvals)
  (loop for (item . others) on (sequence-element-items spec) do
    (apply #'format-docspec-element style target-type item stream keyvals)
    (when others
      (apply #'format-sequence-element-separator
             style target-type spec item (car others) stream keyvals))))

(defgeneric format-sequence-element-separator (style target-type spec
                                                     element1 element2 stream
                                                     &key &allow-other-keys)
  (:method (style target-type spec element1 element2 stream
                  &key &allow-other-keys)
    (declare (ignore style target-type spec element1 element2 stream))))

(defmethod spaceheaded-element ((element standard-sequence))
  (let ((subs (sequence-element-items element)))
    (and subs (spaceheaded-element (car subs)))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :code (standard-code :class standard-doc-element :args (string))
    ((code :initarg :code :reader code-element-string))
  (make-instance 'standard-code :code string))
(set-pprint-dispatch 'standard-code
  (named-function pprint-standard-code
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(code) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))

(def-element :inline (standard-inline :class standard-doc-element
                                      :args (string))
    ((inline :initarg :inline :reader inline-element-string))
  (make-instance 'standard-inline :inline string))
(set-pprint-dispatch 'standard-inline
  (named-function pprint-standard-inline
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(inline) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))
(defmethod spaceheaded-element ((element standard-inline))
  (whitespace-p (elt (inline-element-string element) 0)))

(defclass standard-simple-list-environment (standard-doc-element)
    ((specs :initarg :specs :reader list-element-specs)
     (options :initarg :options :reader list-element-options)
     (env-tag :initarg :env-tag :reader list-element-env-tag)))
(set-pprint-dispatch 'standard-simple-list-environment
  (named-function pprint-standard-simple-list-environment
    (lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(specs options env-tag) do
              (cond
                ((slot-boundp spec slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))

(def-element :itemize (standard-itemize
                       :class standard-simple-list-environment
                       :package package :spec spec
                       :args (options &rest items)) ()
  (make-instance 'standard-itemize
    :options options
    :specs (mapcar (named-function make-standard-itemize-mapper
                     (lambda (x) (compile-element package spec x))) items)
    :env-tag "itemize"))

(def-element :enumerate (standard-enumerate
                         :class standard-simple-list-environment
                         :package package :spec spec
                         :args (options &rest items)) ()
  (make-instance 'standard-enumerate
    :options options
    :specs (mapcar (named-function make-standard-enumerate-mapper
                     (lambda (x) (compile-element package spec x))) items)
    :env-tag "enumerate"))

(def-element :fill-in (standard-fillin-place :class standard-doc-element) ()
  (make-instance 'standard-fillin-place))
(set-pprint-dispatch 'standard-fillin-place
  (named-function pprint-standard-fillin-place
    (lambda (stream spec)
      (declare (ignore spec))
      (format stream "[ placeholder fill-in ]" #| (type-of spec) |# ))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :output-set (standard-outputset-element
                          :class standard-doc-element
                          :args (name &key (style nil style-supp-p)))
    ((name :initarg :name :accessor output-elem-name)
     (style :initarg :style :accessor output-elem-style)
     (style-supp-p :initarg :style-supp-p :accessor output-elem-style-supp-p))
  (make-instance 'standard-outputset-element
    :name name :style style :style-supp-p style-supp-p))
(set-pprint-dispatch 'standard-outputset-element
  (named-function pprint-standard-outputset-element
    (lambda (stream spec)
      (with-accessors ((name output-elem-name)
                       (style output-elem-style)
                       (style-supp-p output-elem-style-supp-p)) spec
        (format stream "[ output-set name=~a style=~a ]" name
                (if style-supp-p style "<unnamed>"))))))

(defmethod format-docspec-element (style target-type
                                         (spec standard-outputset-element)
                                         stream &rest keyvals)
  (declare (ignore target-type))
  (apply #'format-doc stream
         (get-included-outputset-style style (output-elem-style spec) spec)
         (make-instance (output-elem-name spec)) keyvals))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(def-element :file (standard-file-element
                    :class standard-doc-element
                    :args (re-tag file-path &key asdf))
    ((re-tag :initarg :re-tag :accessor file-element-re-tag)
     (file-path :initarg :file-path :accessor file-element-path)
     (asdf :initarg :asdf :accessor file-element-asdf))
  (make-instance 'standard-file-element
    :re-tag re-tag :file-path file-path :asdf asdf))
(set-pprint-dispatch 'standard-file-element
  (named-function pprint-standard-file-element
    (lambda (stream spec)
      (with-accessors ((re-tag file-element-re-tag)
                       (file-path file-element-path)
                       (asdf file-element-asdf)) spec
        (format stream "[ file ~a -- ~a ]" file-path re-tag)))))

(defmethod format-docspec-element (style type (spec standard-file-element)
                                         stream &rest keyvals)
  (with-accessors ((re-tag file-element-re-tag)
                   (file-path file-element-path)
                   (asdf file-element-asdf)) spec
    (cond
      (asdf (setf file-path (asdf:system-relative-pathname asdf file-path))))
    (apply #'format-docspec-element
           style type
           (compile-element *package* nil
                            `(,re-tag
                              ,(with-output-to-string (out)
                                 (with-open-file (in file-path)
                                   (loop for ch = (read-char in nil nil)
                                         while ch
                                         do (format out "~a" ch))))))
           stream keyvals)))

;;; -----------------------------------------------------------------

#+(or allegro sbcl clisp)
(def-contract (standard-elements-style-coverage (style type))
    () ;; options
  (has-method (format-doc (stream style standard-doc-spec) t))
  (has-method (format-doc (stream style output-contents) t))
  (has-method (format-doc (stream style explicit-doc-element) t))
  (has-method (format-docspec (stream style standard-doc-spec t) t))
  (has-method (format-docspec-element (style t standard-fillin-place stream) t))
  (has-method (format-docspec-element (style t standard-plain-text stream) t))
  (has-method (format-docspec-element (style t standard-paragraph-list stream)
                                      t))
  (has-method (format-docspec-element (style t standard-sequence stream) t))
  (has-method (format-docspec-element (style t standard-code stream) t))
  (has-method (format-docspec-element (style t standard-inline stream) t))
  (has-method (format-docspec-element (style t standard-itemize stream) t))
  (has-method (format-docspec-element (style t standard-enumerate stream) t))
  (has-method (format-docspec-element (style t standard-reference stream) t))
  (has-method (format-docspec-element (style t standard-lisp-name stream) t))
  (has-method (format-docspec-element (style t standard-emphasized stream) t))
  (has-method (format-docspec-element (style t standard-outputset-element
                                             stream) t))
  (has-method (format-docspec-element (style t standard-file-element stream)
                                      t)))
