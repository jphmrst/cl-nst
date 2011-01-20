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
(in-package :defdoc)

;;; -----------------------------------------------------------------

(defmacro def-output-class (name-or-spec &body forms)
  (unless (listp name-or-spec)
    (setf name-or-spec (list name-or-spec)))

  ;; Decode the name-or-spec
  (destructuring-bind (name &key (class 'output-contents)
                            title author leader trailer
                            &allow-other-keys)
      name-or-spec

    ;; Define a class, and an intialize-instance method with the
    ;; actual contents of the output unit.
    (loop for form in forms
        for (collector preparation) = (multiple-value-list (eval form))
        collect collector into collectors
        collect preparation into preparations
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
                   (setf (output-contents-contents ,initial)
                     (append ,@collectors)))
                 ',name))))))

;;; -----------------------------------------------------------------

(defgeneric format-output-contents-sep (style stream spec obj1 obj2)
  (:method (style stream spec obj1 obj2)
     (declare (ignore style stream spec obj1 obj2))))

;;; -----------------------------------------------------------------

(defclass output-contents ()
     ((contents :initform nil :initarg :contents :reader contents
                :accessor output-contents-contents)
      (title    :initform nil :initarg :title    :reader unit-title
                :accessor output-contents-title)
      (author   :initform nil :initarg :author   :reader author
                :accessor output-contents-author)
      (leader   :initform nil :initarg :leader   :reader leader
                :accessor output-contents-leader)
      (trailer  :initform nil :initarg :trailer  :reader trailer
                :accessor output-contents-trailer)))

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
    (loop for fn-name in '(title author leader trailer)
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

(defmethod format-doc (stream style (output output-contents))
  (defdoc-debug "format-doc on output-framework ~s~%" output)
  (format-output-leader-material style stream output)
  (let ((*output-nesting-depth* (+ 1 *output-nesting-depth*)))
    (declare (special *output-nesting-depth*))
    (loop for (item . others) on (contents output) do
      (format-output-preitem style stream output item)
      (format-doc stream style item)
      (format-output-postitem style stream output item)
      (when others
        (format-output-contents-sep style stream output item (car others)))))
  (format-output-trailer-material style stream output))

(defgeneric format-output-preitem (style stream output spec)
  (:method (style stream output spec)
     (declare (ignore style stream output spec))))
(defgeneric format-output-postitem (style stream output spec)
  (:method (style stream output spec)
     (declare (ignore style stream output spec))))
(defgeneric format-output-leader-material (style stream output)
  (:method (style stream output) (declare (ignore style stream output)))
  (:method (style stream (output output-contents))
     (format-output-leader-title style stream output)
     (let ((leader (output-contents-leader output)))
       (when leader
         (format-docspec-element style nil leader stream)
         (format-output-leader-sep style stream output)))))
(defgeneric format-output-leader-sep (style stream output)
  (:method (style stream output) (declare (ignore style stream output))))
(defgeneric format-output-leader-title (style stream output)
  (:method (style stream output) (declare (ignore style stream output)))
  (:method (style stream (output output-contents))
    (let ((title (get-output-unit-title output)))
      (when title
        (let ((formatter (cond
                          ((boundp '*output-leader-title-format-string*)
                           (symbol-value '*output-leader-title-format-string*))
                          (t "~a"))))
          (format stream formatter
            (with-output-to-string (str)
              (format-docspec-element style nil title str))))))))
(defgeneric format-output-trailer-material (style stream output)
  (:method (style stream output) (declare (ignore style stream output)))
  (:method (style stream (output output-contents))
     (let ((trailer (output-contents-trailer output)))
       (when trailer (format-docspec-element style nil trailer stream)))))

;;; -----------------------------------------------------------------
;;; Collectors of specs.

(defun collect-target-type (target-name &rest filters)
  (values `(get-specs-by-target-type ',target-name
                                     ,(get-filter-predicate filters))
          nil))

(defun collect-exported-symbols (package &rest filters)
  (values
   `(get-specs-by-package-export ',package ,(get-filter-predicate filters))
   nil))

(defun collect-documented-symbols (package &rest filters)
  (values
   `(get-specs-by-package-allsymbols ',package
                                     ,(get-filter-predicate filters) nil)
   nil))

(defun collect-all-symbols (package &rest filters)
  (values
   `(get-specs-by-package-allsymbols ',package
                                     ,(get-filter-predicate filters) t)
   nil))

(defun collect-output (name-or-spec &rest forms)
  (let ((name) (options))
    (cond (;; There's just a bare symbol.
           (symbolp name-or-spec)
           (setf name name-or-spec options nil))

          ;; There's an output set name with options.
          ((and name-or-spec (listp name-or-spec) (car name-or-spec)
                (symbolp (car name-or-spec))
                (not (keywordp (car name-or-spec))))
           (setf name (car name-or-spec) options (cdr name-or-spec)))

          ;; There's output set options, but no name.
          ((and name-or-spec (listp name-or-spec) (car name-or-spec)
                (keywordp (car name-or-spec)))
           (setf name (gensym) options name-or-spec)))
    (values `(list (make-instance ',name))
            `(def-output-class (,name ,@options) ,@forms))))

(defun collect-named-output (name)
  (values `(list (make-instance ',name)) nil))

;;; -----------------------------------------------------------------
;;; Literal insertion of docspec forms.

(defmacro collect-doc (options &rest forms)
  `(values `(list (make-doc-holder ',',options ',',forms)) nil))

(defclass explicit-doc-element (labeled)
  ((docspec :initarg :docspec :accessor docspec)))

(defun make-doc-holder (options spec-forms)
  (declare (ignore options))
  (let ((result (make-instance 'explicit-doc-element)))
    (setf (docspec result) (compile-element *package* result spec-forms))
    result))

(defmethod format-doc (stream style (spec explicit-doc-element))
  (format-docspec-element style nil (docspec spec) stream))

;;; -----------------------------------------------------------------
;;; Grouping contents.

(defmacro collect-groups-by-label ((label-name &key
                                               (package nil package-supp-p)
                                               groups
                                               &allow-other-keys) &body forms)
  `(values ',(get-label-grouper-call label-name groups package package-supp-p
                                     forms)
           nil))

(defun get-label-grouper-call (label-name groups package package-supp-p forms)
  (let ((adjusted-groups
         (cond
           (package-supp-p
            (loop for g in groups
                  collect (cond
                            ((and (symbolp g) (eq *package* (symbol-package g)))
                             (intern (symbol-name g) package))
                            ((and (consp g) (car g) (symbolp (car g))
                                  (not (keywordp (car g)))
                                  (eq *package* (symbol-package (car g))))
                             (cons (intern (symbol-name (car g)) package)
                                   (cdr g)))
                            (t g))))
           (t groups))))
    `(group-list-by-label
      :group-specs ,adjusted-groups
      :grouping-label ',label-name
      :generating-thunk (named-function default-generating-thunk
                          (lambda ()
                            ,(case (length forms)
                               ((1) (car forms))
                               (otherwise
                                `(append ,@(loop for form in forms
                                               collect (eval form))))))))))

(defun group-list-by-label (&key (group-specs nil groups-supp-p)
                                 grouping-label
                                 (generating-thunk #'(lambda () nil)))
  (let ((contents (funcall generating-thunk)))
    (let* ((allowed nil)
           (group-order-spec (make-hash-table :test 'eq))
           (group-title-spec (make-hash-table :test 'eq))
           (group-leader-spec (make-hash-table :test 'eq))
           (group-trailer-spec (make-hash-table :test 'eq))
           (grouping-label-def (get-labeldef grouping-label))
           (group-hash (make-hash-table :test 'eq)))
      (loop for group-spec in group-specs do
        (cond ((symbolp group-spec) (push group-spec allowed))
              ((listp group-spec)
               (let ((name (pop group-spec)))
                 (push name allowed)
                 (destructuring-bind (&key (order nil order-supp-p)
                                           (title nil title-supp-p)
                                           (leader nil leader-supp-p)
                                           (trailer nil trailer-supp-p)
                                           &allow-other-keys) group-spec
                   (when order-supp-p
                     (setf (gethash name group-order-spec) order))
                   (when title-supp-p
                     (setf (gethash name group-title-spec)
                           (compile-element *package* nil title)))
                   (when leader-supp-p
                     (setf (gethash name group-leader-spec)
                           (compile-element *package* nil leader)))
                   (when trailer-supp-p
                     (setf (gethash name group-trailer-spec)
                           (compile-element *package* nil trailer))))))
              (t (error "Expected symbol or list: ~s" group-spec))))
      (setf allowed (reverse allowed))

      ;; Sort the contents into groups.
      (defdoc-debug "Organizing output doc groups~%")
      (loop for item in contents do
        (let ((this-group (label-value item grouping-label)))
;;;           (when (and (null this-group)
;;;                      (standard-output-framework-default-group-supp-p
;;;                       output-framework))
;;;             (setf this-group (standard-output-framework-default-group
;;;                               output-framework)))
          (push item (gethash this-group group-hash))
          (defdoc-debug " - [~s ~s] --> Group ~s~%"
            (docspec-target-type item) (docspec-self item)
            this-group)))

      ;; If we have a list of groups, prune any that aren't included.
      (when groups-supp-p
        (defdoc-debug "Checking against allowed groups list ~s~%" allowed)
        (loop for group-name being the hash-keys of group-hash do
          (unless (member group-name allowed :test 'eq)
            (defdoc-debug "Dropping unused group ~s~%" group-name)
            (when (gethash group-name group-hash)
              (warn "Group ~s nonempty, but excluded from output" group-name))
            (remhash group-name group-hash))))

      ;; Extract the actual list of group names.
      (let ((group-name-list
             (apply #'extract-and-refine-group-names group-hash
                    `(,@(when groups-supp-p `(:allowed-list ,allowed))))))
        (loop for group in group-name-list
            collect
              (let ((spec-list (gethash group group-hash))
                    (item-order (gethash group group-order-spec)))
                (cond
                 (item-order
                  (setf spec-list
                    (sort spec-list
                          (named-function output-framework-specs-sorter
                            (lambda (x y)
                              (let ((xp (position (docspec-self x)
                                                  item-order :test 'eq))
                                    (yp (position (docspec-self y)
                                                  item-order :test 'eq)))
                                (cond
                                 ((and (numberp xp) (numberp yp))
                                  (< xp yp))
                                 ((and (numberp xp) (null yp)) t)
                                 (t nil)))))))))
                (defdoc-debug " --> group ~s ~s~%" group spec-list)

                (let ((optionals nil))
                (when (gethash group group-title-spec)
                  (push (gethash group group-title-spec) optionals)
                  (push :title optionals))
                (when (gethash group group-leader-spec)
                  (push (gethash group group-leader-spec) optionals)
                  (push :leader optionals))
                (when (gethash group group-trailer-spec)
                  (push (gethash group group-trailer-spec) optionals)
                  (push :trailer optionals))

                (apply #'make-instance 'grouped-output-contents
                       :labeldef grouping-label-def :group group
                       :contents spec-list
                       optionals))))))))

(defun extract-and-refine-group-names (group-hash
                                       &key (allowed-list
                                             nil allowed-list-supp-p))
  (let ((result (loop for group-name being the hash-keys of group-hash
                    collect group-name)))

    (defdoc-debug "Group-name-list [1] ~s~%" result)

    ;; Should we sort the groups?
    (cond

     ;; If we have an explicit sorter, apply it.
     ;;
     ;; FILL IN

     ;; If we have a group list specified, mimic it.
     (allowed-list-supp-p
      (setf result (loop for g in allowed-list if (member g result) collect g)))

     ;; Otherwise we just leave it alone.
     )

    (defdoc-debug "Group-name-list [2] ~s~%" result)

    result))

(defclass grouped-output-contents (output-contents)
     ((labeldef :initform nil :initarg :labeldef :reader labeldef)
      (group    :initform nil :initarg :group    :reader group)))
(defmethod get-output-unit-title ((o grouped-output-contents))
  (let ((result (call-next-method)))
    (cond
     (result result)
     (t (with-accessors ((label labeldef) (group group)) o
          (cond
           ((get-label-section-title-supp-p label group o)
            (get-label-section-title label group o))
           (t nil)))))))
(defmethod pprint-output-contents-fields append ((o grouped-output-contents))
  (loop for fn-name in '(group)
    if (and (slot-boundp o fn-name) (slot-value o fn-name)) collect fn-name))

(defmethod title ((o grouped-output-contents))
  (with-accessors ((label labeldef) (group group)) o
    (cond
      ((get-label-section-title-supp-p label group o)
       (get-label-section-title label group o))
      (t (call-next-method)))))
