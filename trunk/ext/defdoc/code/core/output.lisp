;;; File output.lisp
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
(in-package :defdoc-core)

;;; -----------------------------------------------------------------

(defmacro def-output-class (name-or-spec &body forms)
  (unless (listp name-or-spec)
    (setf name-or-spec (list name-or-spec)))

  ;; Decode the name-or-spec
  (destructuring-bind (name &key (class 'output-contents)
                                 short-title title author leader trailer
                       &allow-other-keys)
      name-or-spec

    ;; Define a class, and an intialize-instance method with the
    ;; actual contents of the output unit.
    (loop for form in forms
        for (collector preparation) = (multiple-value-list (eval form))
        collect collector into collectors
        append preparation into preparations
        finally
          (let ((initial (gensym)))
            (return-from def-output-class
              `(progn
                 (defclass ,name (,class) ())
                 ,@preparations
                 (defmethod initialize-instance :after ((,initial ,name)
                                                        &key &allow-other-keys)
                   ,@(when leader
                       `((setf (output-contents-leader ,initial)
                               (compile-element *package* nil ',leader))))
                   ,@(when trailer
                       `((setf (output-contents-trailer ,initial)
                               (compile-element *package* nil ',trailer))))
                   ,@(when author
                       `((setf (output-contents-author ,initial)
                               (compile-element *package* nil ',author))))
                   ,@(when title
                       `((setf (output-contents-title ,initial)
                               (compile-element *package* nil ',title))))
                   ,@(when short-title
                       `((setf (output-contents-short-title ,initial)
                               (compile-element *package* nil ',short-title))))
                   (setf (output-contents-contents ,initial)
                         (append ,@collectors)))
                 ',name))))))

(defgeneric write-output (style output-name directory file-name-root
                                &key &allow-other-keys)
  (:method :around (style output-name directory file-name-root
                          &key &allow-other-keys)
    (declare (ignore style output-name directory file-name-root))
    (let ((*print-length* nil)
          (*print-lines* nil)
          (*print-level* nil)
          (*print-pretty* t)
          (*print-circle* nil))
      (call-next-method)))
  (:method ((symbol symbol) output-name directory file-name-root
            &rest keyvals &key &allow-other-keys)
    (apply #'write-output (make-instance symbol)
           output-name directory file-name-root keyvals)))
(defgeneric get-filename-extension (style output-name directory file-name-root
                                          &key &allow-other-keys))

;;; -----------------------------------------------------------------

(defgeneric format-default-output-contents-sep (style stream spec obj1 obj2)
  (:method (style stream spec obj1 obj2)
     (declare (ignore style stream spec obj1 obj2))))

(defgeneric format-output-contents-sep (style stream spec obj1 obj2
                                              &key &allow-other-keys)
  (:method (style stream spec obj1 obj2 &key &allow-other-keys)
    (format-default-output-contents-sep style stream spec obj1 obj2)))

;;; -----------------------------------------------------------------

(defclass output-contents ()
  ((contents :initform nil :initarg :contents :reader contents
             :accessor output-contents-contents)
   (short-title :initform nil :initarg :short-title :reader unit-short-title
                :accessor output-contents-short-title)
   (title   :initform nil :initarg :title    :reader unit-title
            :accessor output-contents-title)
   (author  :initform nil :initarg :author   :reader author
            :accessor output-contents-author)
   (leader  :initform nil :initarg :leader   :reader leader
            :accessor output-contents-leader)
   (trailer :initform nil :initarg :trailer  :reader trailer
            :accessor output-contents-trailer)
   (style   :initform nil :initarg :style :reader output-contents-style)))

(defgeneric get-output-unit-short-title (o)
  (:method (o) (declare (ignore o)))
  (:method ((o output-contents)) (unit-short-title o)))
(defgeneric get-output-unit-title (o)
  (:method (o) (declare (ignore o)))
  (:method ((o output-contents)) (unit-title o)))
(defgeneric get-output-unit-author (o)
  (:method (o) (declare (ignore o)))
  (:method ((o output-contents)) (author o)))
(defgeneric get-output-unit-leader (o)
  (:method (o) (declare (ignore o)))
  (:method ((o output-contents)) (leader o)))
(defgeneric get-output-unit-trailer (o)
  (:method (o) (declare (ignore o)))
  (:method ((o output-contents)) (trailer o)))

(defgeneric pprint-output-contents-fields (o)
  (:method-combination append :most-specific-last)
  (:method append (o) (declare (ignore o)))
  (:method append ((o output-contents))
    (loop for fn-name in '(title author style leader trailer)
          if (and (slot-boundp o fn-name) (slot-value o fn-name))
            collect fn-name)))
(set-pprint-dispatch 'output-contents
  (named-function pprint-output-contents
    (lambda (stream spec)
      (let ((*pprint-short-spec* t))
        (declare (special *pprint-short-spec*))
        (pprint-logical-block (stream '(1))
          (format stream "{ OUTPUT")
          (pprint-logical-block
              (stream (loop for fn-name in (pprint-output-contents-fields spec)
                            if (slot-boundp spec fn-name)
                              collect fn-name))
            (loop for fn-name = (pprint-pop) while fn-name do
              (format stream " ~a=~s" fn-name (slot-value spec fn-name))
              (pprint-exit-if-list-exhausted)
              (pprint-newline :fill stream)))
          (loop for item in (contents spec) do
            (pprint-newline :mandatory stream)
            (princ "  - " stream)
            (write item :stream stream))
          (pprint-newline :mandatory stream)
          (princ "}" stream))))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defparameter *output-nesting-depth* 0)

(defun format-output-contents-actual (stream style output &rest keyargs
                                             &key &allow-other-keys)
  (pprint-logical-block (stream '(1 2))
    (apply #'format-output-leader-material style stream output keyargs)
    (apply #'format-doc-content-items stream style output keyargs)
    (apply #'format-output-trailer-material style stream output keyargs)))

(defmethod format-doc (stream style (output output-contents)
                              &rest keyargs &key &allow-other-keys)
  (defdoc-debug "format-doc on output-contents ~s~%" (type-of output))
  (apply #'format-output-contents-actual stream
         (resolve-outputset-style style output) output keyargs))

(defgeneric format-doc-content-items (stream style output
                                             &key &allow-other-keys)
  (:method (stream style (output output-contents) &rest keyargs)
    (let ((*output-nesting-depth* (+ 1 *output-nesting-depth*)))
      (declare (special *output-nesting-depth*))
      (loop for (item . others) on (contents output) do
        (apply #'format-output-preitem style stream output item keyargs)
        (apply #'format-doc-content-item stream style item keyargs)
        (apply #'format-output-postitem style stream output item keyargs)
        (when others
          (apply #'format-output-contents-sep
                 style stream output item (car others) keyargs))))))

(defgeneric format-doc-content-item (stream style output &key &allow-other-keys)
  (:method (stream style output &rest keyargs)
    (apply #'format-doc stream style output keyargs))
  (:method (stream style (spec doc-spec) &rest keyargs)
    (apply #'format-docspec
           stream style spec (docspec-target-type spec) keyargs)))

(defgeneric format-output-preitem (style stream output spec
                                         &key &allow-other-keys)
  (:method (style stream output spec &key &allow-other-keys)
     (declare (ignore style stream output spec))))
(defgeneric format-output-postitem (style stream output spec
                                          &key &allow-other-keys)
  (:method (style stream output spec &key &allow-other-keys)
     (declare (ignore style stream output spec))))
(defgeneric format-output-leader-material (style stream output
                                                 &key &allow-other-keys)
  (:method (style stream output &key &allow-other-keys)
    (declare (ignore style stream output)))
  (:method (style stream (output output-contents) &rest keyargs)
     (defdoc-debug "format-output-leader-material on output-contents ~s~%"
        (type-of output))
     (apply #'format-output-leader-title style stream output keyargs)
     (let ((leader (output-contents-leader output)))
       (when leader
         (format-output-leader-docspec style leader stream)
         (format-output-leader-sep style stream output)))))

(defgeneric format-output-leader-docspec (style leader stream)
  (:method (style leader stream)
    (format-docspec-element style nil leader stream)))
(defgeneric format-output-leader-sep (style stream output)
  (:method (style stream output) (declare (ignore style stream output))))
(defgeneric format-output-leader-title (style stream output
                                              &key &allow-other-keys)
  (:method (style stream output &key &allow-other-keys)
    (declare (ignore style stream output)))
  (:method (style stream (output output-contents) &rest keyargs)
    (let ((title (get-output-unit-title output)))
      (when title
        (let ((formatter
               (cond
                 ((boundp '*output-leader-title-format-string*)
                  (symbol-value '*output-leader-title-format-string*))
                 (t "~a"))))
          (format stream formatter
            (with-output-to-string (str)
              (apply #'format-docspec-element
                     style nil title str keyargs))))))))

(defgeneric format-output-trailer-material (style stream output
                                                  &key &allow-other-keys)
  (:method (style stream output &key &allow-other-keys)
    (declare (ignore style stream output)))
  (:method (style stream (output output-contents) &key &allow-other-keys)
     (let ((trailer (output-contents-trailer output)))
       (when trailer
         (format-output-trailer-docspec style trailer stream)))))
(defgeneric format-output-trailer-docspec (style trailer stream)
  (:method (style trailer stream)
    (format-docspec-element style nil trailer stream)))

(defgeneric get-included-outputset-style (outer-style inner-style contained)
  (:method :around (outer-style inner-style contained)
    (declare (ignore outer-style inner-style contained))
    (let ((result (call-next-method)))
      (cond
        ((symbolp result) (make-instance result))
        (t result))))
  (:method (outer-style inner-style contained)
    (declare (ignore contained))
    (cond
      (inner-style inner-style)
      (t outer-style))))

(defun resolve-outputset-style (outer-style inner-output-container)
  (get-included-outputset-style outer-style
                                (output-contents-style inner-output-container)
                                inner-output-container))

(defgeneric format-docspec-aftermatter-mark (style mark stream
                                                   &key &allow-other-keys)
  (:method (style mark stream &key &allow-other-keys)
    (declare (ignore style mark stream))))
