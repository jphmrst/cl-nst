;;; File collectors.lisp
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
;;; Collectors of specs.

(defun collect-target-type (target-name &rest filters)
  (values `(get-specs-by-target-type ',target-name
                                     ,(get-filter-predicate filters))
          nil))

(defmacro collect-symbols (package symbol-list &rest filters)
  (unless (listp symbol-list)
    (setf symbol-list (list symbol-list)))
  `(values
    `(let ((p (find-package ',',package)))
       (get-symbol-list-specs (loop for s in ',',symbol-list
                                  collect (intern (symbol-name s) p))
                              ,',(get-filter-predicate filters)))
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

(defmacro collect-output (name-or-spec &rest forms)
  (let ((name) (options))
    (cond (;; There's a list of some sort
           (listp name-or-spec)

           (cond
            ((and (car name-or-spec) (symbolp (car name-or-spec))
                  (not (keywordp (car name-or-spec))))
             (setf name (car name-or-spec) options (cdr name-or-spec)))

            (t (setf name (gensym) options name-or-spec))))

          (;; There's just a bare symbol.
           (symbolp name-or-spec)
           (setf name name-or-spec options nil))

          (t (error "Expected symbol or list, got ~s" name-or-spec)))

    `(values `(list (make-instance ',',name))
             `((def-output-class (,',name ,@',options) ,@',forms)))))

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

(defmethod format-doc (stream style (spec explicit-doc-element)
                              &key &allow-other-keys)
  (format-docspec-element style nil (docspec spec) stream))

;;; -----------------------------------------------------------------
;;; Grouping contents.

(defmacro collect-groups-by-label ((label-name &key
                                               (package nil package-supp-p)
                                               groups
                                               exhaustive
                                               &allow-other-keys) &body forms)
  `(values ',(get-label-grouper-call label-name groups package package-supp-p
                                     exhaustive forms)
           nil))

(defun get-label-grouper-call (label-name groups package package-supp-p
                                          exhaustive forms)
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
      :exhaustive ,exhaustive
      :group-specs ,adjusted-groups :grouping-label ',label-name
      :generating-thunk (named-function default-generating-thunk
                          (lambda ()
                            ,(case (length forms)
                               ((1) (eval (car forms)))
                               (otherwise
                                `(append ,@(loop for form in forms
                                               collect (eval form))))))))))

(defun group-list-by-label (&key (group-specs nil groups-supp-p)
                                 grouping-label
                                 exhaustive
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
                                           &allow-other-keys)
                     group-spec
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
            (when (and (gethash group-name group-hash) exhaustive)
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
                  (labels ((position-of (z)
                             (position z item-order :test 'eq))

                           (output-framework-specs-sorter (x y)
                             (let* ((xp (position-of (docspec-self x)))
                                    (yp (position-of (docspec-self y)))
                                    (xap (let ((xa (label-value x 'anchor)))
                                           (when xa (position-of xa))))
                                    (yap (let ((ya (label-value y 'anchor)))
                                           (when ya (position-of ya))))
                                    (num-xp (numberp xp))
                                    (num-yp (numberp yp))
                                    (num-xap (numberp xap))
                                    (num-yap (numberp yap)))
                               (cond
                                ((and num-xp num-yp)   (< xp yp))
                                ((and num-xp num-yap)  (<= xp yap))
                                ((and num-xap num-yp)  (< xap yp))
                                (num-xp                t)
                                (num-yp                nil)
                                ((and num-xap num-yap) (< xap yap))
                                (num-xap               t)
                                (num-yap               nil)
                                (t                     nil)))))
                    (setf spec-list
                          (sort spec-list #'output-framework-specs-sorter)))))
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
  ((labeldef :initform nil :initarg :labeldef :reader get-grouped-output-labeldef)
   (group    :initform nil :initarg :group    :reader get-grouped-output-group)))
(defmethod get-output-unit-title ((o grouped-output-contents))
  (let ((result (call-next-method)))
    (cond
     (result result)
     (t (with-accessors ((label get-grouped-output-labeldef)
                         (group get-grouped-output-group)) o
          (cond
            ((get-label-section-title-supp-p label group o)
             (get-label-section-title label group o))
           (t nil)))))))
(defmethod pprint-output-contents-fields append ((o grouped-output-contents))
  (loop for fn-name in '(group)
    if (and (slot-boundp o fn-name) (slot-value o fn-name)) collect fn-name))

(defmethod title ((o grouped-output-contents))
  (with-accessors ((label get-grouped-output-labeldef)
                   (group get-grouped-output-group)) o
    (cond
     ((get-label-section-title-supp-p label group o)
       (get-label-section-title label group o))
      (t (call-next-method)))))
