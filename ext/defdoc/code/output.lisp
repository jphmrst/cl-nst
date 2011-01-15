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

(defgeneric format-output-pregroup (style stream output-framework label group)
  (:method (style stream output-framework label group)
     (declare (ignore style stream output-framework label group))))
(defgeneric format-output-prespec (style stream output-framework spec)
  (:method (style stream output-framework spec)
     (declare (ignore style stream output-framework spec))))
(defgeneric format-output-postspec (style stream output-framework spec)
  (:method (style stream output-framework spec)
     (declare (ignore style stream output-framework spec))))
(defgeneric format-output-spec-sep (style stream output-framework spec1 spec2)
  (:method (style stream output-framework spec1 spec2)
     (declare (ignore style output-framework spec1 spec2))
     (format stream "\\par ")))
(defgeneric format-output-postgroup (style stream output-framework group)
  (:method (style stream output-framework group)
     (declare (ignore style stream output-framework group))))
(defgeneric format-output-group-sep (style stream output-framework
                                           group1 group2)
  (:method (style stream output-framework group1 group2)
     (declare (ignore style output-framework group1 group2))
     (format stream "\\par ")))

(defgeneric is-output-framework-class (cls)
  (:method (c) (declare (ignore c)) nil)
  (:method ((sym symbol))
    (let ((cls (find-class sym nil)))
      (when cls (is-output-framework-class cls)))))
(defgeneric is-output-framework (obj)
  (:method (o) (declare (ignore o)) nil))

;;; -----------------------------------------------------------------

(defgeneric get-specs-for-output-framework (spec)
  (:method (other)
     (declare (ignore other))
     nil)
  (:method ((spec symbol))
     (get-specs-for-output-framework (make-instance spec))))

(defgeneric finalize-output-framework (spec)
  (:method (spec)
     (declare (ignore spec))))

(defun get-output-framework-forms (label-name forms
                                              &rest keyvals &key
                                              (package nil)
                                              (class 'grouped-output-framework)
                                              &allow-other-keys)
;;;  (unless package-supp-p
;;;    (setf package (symbol-package label-name)))
  `((defclass ,label-name (,class) ())
    (defmethod is-output-framework-class ((cls (eql (find-class ',label-name))))
      t)
    (defmethod is-output-framework ((o ,label-name)) t)
    (defmethod initialize-instance :after ((spec ,label-name)
                                           &key &allow-other-keys)
      (setf (slot-value spec 'name) ',label-name)
      ,@(loop while keyvals
            append (let ((key (pop keyvals))
                         (val (pop keyvals)))
                     (unless (eq key :class)
                       `((process-standard-output-framework-form
                          spec ,package ,key (list ,val))))))
      (finalize-output-framework spec))
    ,@(block from-forms
        (loop for form in forms
              for (collect-call init-forms) = (multiple-value-list (eval form))
              collect collect-call into append-arg-forms
              append init-forms into additional-toplevel
              finally
           (return-from from-forms
             `(,@additional-toplevel
               (defmethod get-specs-for-output-framework
                   ((spec ,label-name))
                 ,(cond
                   ((eql 1 (length append-arg-forms))
                    (car append-arg-forms))
                   (t `(append ,@(loop for form in append-arg-forms
                                     collect (eval form))))))))))))

(defmacro def-output-framework (label-name-or-spec &body forms)
  (let (label-name keyvals)
    (cond
     ((symbolp label-name-or-spec)
      (setf label-name label-name-or-spec))
     (t (destructuring-bind (the-label-name &rest the-keyvals
                                            &key &allow-other-keys)
            label-name-or-spec
          (setf label-name the-label-name keyvals the-keyvals))))
    `(progn ,@(apply #'get-output-framework-forms label-name forms keyvals))))

(defun get-output-framework (name)
  (when (find-class name nil) (make-instance name)))

(defun get-output-frameworks-collector (names)
  (named-function get-output-frameworks-collector-thunk
    (lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for name-or-spec in names do
            (with-name-and-filters (name filters name-or-spec)
              (let ((spec (get-output-framework name)))
                (when (funcall (get-filter-predicate filters) spec)
                  (setf (gethash spec result) t)))))
      result)))

;;; -----------------------------------------------------------------

(defvar *output-framework-class* 'standard-output-framework)
(defgeneric get-output-framework-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *output-framework-class*))

(defclass output-framework (labeled)
     ((name :initarg :name :reader output-framework-name)
      (default-style :initarg :default-style :reader output-framework-style)))

(defclass standard-output-framework (output-framework)
     ((collector :initform (named-function
                               standard-output-framework-collector-default
                             (lambda (&optional (result
                                                 (make-hash-table :test 'eq)))
                               result))
                 :accessor standard-output-framework-collector)
      (grouping-label :reader standard-output-framework-grouping-label)
      (groups :reader standard-output-framework-groups)
      (group-config-args :reader standard-output-framework-group-config-args
                         :initform (make-hash-table :test 'eq))
      (default-group :reader standard-output-framework-default-group)
      (title  :reader standard-output-framework-title)
      (author :reader standard-output-framework-author)))

(def-slot-supp-predicates standard-output-framework
    ((standard-output-framework-grouping-label-supp-p grouping-label)
     (standard-output-framework-groups-supp-p         groups)
     (standard-output-framework-title-supp-p          title)
     (standard-output-framework-author-supp-p         author)
     (standard-output-framework-default-group-supp-p  default-group)))

(defun invoke-collector (output)
  (loop for spec being the hash-keys
        of (funcall (standard-output-framework-collector output))
        collect spec))

(defmethod format-doc (stream style
                              (output-framework standard-output-framework))
  (defdoc-debug "format-doc on output-framework ~s~%" output-framework)

  (let ((contents (invoke-collector output-framework))
        (oname (output-framework-name output-framework)))

    ;; Are we going to group or sort the contents?
    (cond
     ;; Group them (and maybe sort).
     ((standard-output-framework-grouping-label-supp-p output-framework)

      ;; Set up the groups.
      (let* ((grouping-label (standard-output-framework-grouping-label
                              output-framework))
             (grouping-label-def (get-labeldef grouping-label))
             (group-hash (make-hash-table :test 'eq)))
        (defdoc-debug "Organizing output doc groups~%")
        (loop for item in contents do
          (let ((this-group (label-value item grouping-label)))
            (when (and (null this-group)
                       (standard-output-framework-default-group-supp-p
                        output-framework))
              (setf this-group (standard-output-framework-default-group
                                output-framework)))
            (push item (gethash this-group group-hash))
            (defdoc-debug " - ~s --> Group ~s~%"
                (docspec-self item) this-group)))

        ;; If we have a list of groups, prune any that aren't included.
        (when (standard-output-framework-groups-supp-p output-framework)
          (let ((allowed (standard-output-framework-groups output-framework)))
            (loop for group-name being the hash-keys of group-hash do
                  (unless (member group-name allowed :test 'eq)
                    (defdoc-debug "Dropping unused group ~s~%" group-name)
                    (when (gethash group-name group-hash)
                      (warn "Group ~s nonempty, but excluded from output"
                            group-name))
                    (remhash group-name group-hash)))))

        ;; Extract the actual list of group names.
        (let ((group-name-list (loop for group-name being the hash-keys
                                   of group-hash collect group-name)))

          ;; Should we sort the groups?
          (cond

            ;; If we have an explicit sorter, apply it.
            ;;
            ;; FILL IN

            ;; If we have a group list specified, mimic it.
            ((standard-output-framework-groups-supp-p output-framework)
             (setf group-name-list
                   (loop for g in (standard-output-framework-groups
                                           output-framework)
                         if (member g group-name-list)
                           collect g)))

            ;; Otherwise we just leave it alone.
            )

          ;; Groups are set up, so go through them and print them.
          (loop for (group . next-groups) on group-name-list do
            (format-output-pregroup style stream output-framework
                   grouping-label-def group)
            (let ((*sectioning-level* (+ 1 *sectioning-level*)))
              ;; Should we sort the contents of each group?
              (when (get-label-section-order-supp-p grouping-label-def
                                                    style group oname)
                (let ((elements (gethash group group-hash))
                      (item-order (get-label-section-order grouping-label-def
                                                           style group oname)))
                  (setf (gethash group group-hash)
                        (sort elements
                              (named-function
                                  format-standard-output-framework-sorter
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

              (loop for (spec . other-specs) on (gethash group group-hash) do
                (format-output-prespec style stream output-framework spec)
                (format-doc stream style spec)
                (format-output-postspec style stream output-framework spec)
                (when other-specs
                  (format-output-spec-sep style stream output-framework
                                          spec (car other-specs)))))
            (format-output-postgroup style stream output-framework group)
            (when next-groups
              (format-output-group-sep style stream output-framework
                                       group (car next-groups)))))))

     ;; Just sort them, but not group.
     (t

      (loop for item in contents do (format-doc stream style item))))))

(set-pprint-dispatch 'standard-output-framework
  (named-function pprint-standard-output-framework
    (lambda (stream output)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~s ~s"
          (type-of output)
          (output-framework-name output))
        (let ((props (label-values output)))
          (when (< 0 (hash-table-count props))
            (format stream "~:@_  - properties:~:@_    ")
            (pprint-logical-block (stream
                                   (loop for label being the hash-keys of props
                                       collect label))
              (loop for label = (pprint-pop)
                  for value = (gethash label props)
                  do
                    (format stream "~s ~s" label value)
                    (pprint-exit-if-list-exhausted)
                    (pprint-newline :mandatory)))))
        (loop for slot in '(default-style grouping-label groups)
            do
              (cond
                ((slot-boundp output slot)
                 (format stream "~:@_  - ~a ~w" slot (slot-value output slot)))
                (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]")))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun set-single-assign-slot (output-framework slot-name slot-value)
     (when (slot-boundp output-framework slot-name)
       (error "Multiple settings of ~s for output framework ~s"
              slot-name (output-framework-name output-framework)))
     (setf (slot-value output-framework slot-name) slot-value))

(defgeneric default-standard-output-framework-form-processor (form-head
                                                              output-framework
                                                              package form-args)

  (:method (form-head output-framework package form-args)
     (declare (ignore package))
     (error "Unrecognized form (~a~{ ~s~}) in def-output-framework ~s"
            form-head form-args (doc-label-name output-framework)))

  (:method ((form-head (eql :style))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (when (slot-boundp output-framework 'default-style)
       (error "Multiple styles given for output framework ~s"
              (output-framework-name output-framework)))
     (destructuring-bind (style-name) form-args
       (setf (slot-value output-framework 'default-style) style-name)))

  (:method ((form-head (eql :property-values))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (collectors-unitef
      (standard-output-framework-collector output-framework)
      (loop for (label value) in form-args
            collect (filter-collector #'all-specs-collector
                                        (named-function
                                              property-values-collector
                                          (lambda (spec)
                                            (eql (label-value spec label)
                                                 value)))))))

  (:method ((form-head (eql :grouping-label))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (label) form-args
       (set-single-assign-slot output-framework 'grouping-label label)))

  (:method ((form-head (eql :groups))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (let ((group-names nil)
           (args-hash
            (standard-output-framework-group-config-args output-framework)))
       (loop for arg in form-args do
         (cond
          ((and (consp arg) (symbolp (car arg)))
           (let ((this-name (car arg)))
             (push this-name group-names)
             (setf (gethash this-name args-hash) (cdr arg))))
          ((symbolp arg)
           (push arg group-names)
           (setf (gethash arg args-hash) nil))
          (t (error "Expected symbol or symbol-headed list, got ~s" arg))))
       (set-single-assign-slot output-framework 'groups (reverse group-names))))

  (:method ((form-head (eql :default-group))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (set-single-assign-slot output-framework 'default-group form-args))

  (:method ((form-head (eql :title))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (title-string) form-args
       (set-single-assign-slot output-framework 'title title-string)))

  (:method ((form-head (eql :author))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (author-string) form-args
       (set-single-assign-slot output-framework 'author author-string)))

  (:method ((form-head (eql :with-output))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (collector-unitef (standard-output-framework-collector output-framework)
                       (get-output-frameworks-collector form-args)))

  (:method ((form-head (eql :exported-symbols))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (collector-unitef (standard-output-framework-collector output-framework)
                       (get-packages-exported-collector form-args t)))

  (:method ((form-head (eql :target-type))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     ;; (format t "** A ** :target-type (~{~s~^ ~})~%" form-args)
     (collector-unitef (standard-output-framework-collector output-framework)
                       (get-target-types-collector form-args)))

  (:method ((form-head (eql :documented-symbols))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (collector-unitef (standard-output-framework-collector output-framework)
                        (get-packages-symbols-collector form-args nil)))

  (:method ((form-head (eql :all-symbols))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (collector-unitef (standard-output-framework-collector output-framework)
                        (get-packages-symbols-collector form-args t))))

(defgeneric process-standard-output-framework-form (output-framework package
                                                    form-head form-args)
  (:method (output-framework package form-head form-args)
     (default-standard-output-framework-form-processor
         form-head output-framework package form-args)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defclass grouped-output-framework (standard-output-framework)
     ((group-specs-supp-p :initarg :group-specs-supp-p
                          :reader group-specs-supp-p)
      (group-specs      :initarg :group-specs      :reader group-specs)
      (base-label       :initarg :base-label       :reader base-label)
      (generating-thunk :initarg :generating-thunk :reader generating-thunk)))

(defgeneric format-grouped-output-sep (style stream spec obj1 obj2)
  (:method (style stream spec obj1 obj2)
     (declare (ignore style stream spec obj1 obj2))))

(defmethod format-doc (stream style (grouper grouped-output-framework))

  (let ((contents (get-specs-for-output-framework grouper)))
    (loop for (group . next-groups) on contents do
      (format-doc stream style group)
      (when next-groups
        (format-grouped-output-sep style stream grouper
                                   group (car next-groups))))))

(defmethod get-specs-for-output-framework ((output-framework
                                            grouped-output-framework))
  (let ((contents (funcall (generating-thunk output-framework))))
    ;;; TO DO --- adapt the code in the format-doc above (the first
    ;;; block, for grouping) to here, debug with the
    ;;; new-def-output-framework in doc.lisp.
    ;;;
    ;;; THEN --- transform this code to directly expand into the macro
    ;;; below, and get rid of this defclass/defmethod.

    (with-accessors ((grouping-label base-label)
                     (group-specs group-specs)
                     (groups-supp-p  group-specs-supp-p)) output-framework
      (let* ((allowed nil)
             (group-order-spec (make-hash-table :test 'eq))
             (grouping-label-def (get-labeldef grouping-label))
             (group-hash (make-hash-table :test 'eq)))
        (loop for group-spec in group-specs do
          (cond ((symbolp group-spec) (push group-spec allowed))
                ((listp group-spec)
                 (let ((name (pop group-spec)))
                   (push name allowed)
                   (destructuring-bind (&key (order nil order-supp-p)
                                             &allow-other-keys) group-spec
                     (when order-supp-p
                       (setf (gethash name group-order-spec) order)))))
                (t (error "Expected symbol or list: ~s" group-spec))))
        (setf allowed (reverse allowed))
        (defdoc-debug "Organizing output doc groups~%")
        (loop for item in contents do
          (let ((this-group (label-value item grouping-label)))
            (when (and (null this-group)
                       (standard-output-framework-default-group-supp-p
                        output-framework))
              (setf this-group (standard-output-framework-default-group
                                output-framework)))
            (push item (gethash this-group group-hash))
            (defdoc-debug " - ~s --> Group ~s~%"
                item ; (docspec-self item) ; item
              this-group)))

        ;; If we have a list of groups, prune any that aren't included.
        (when groups-supp-p
          (loop for group-name being the hash-keys of group-hash do
            (unless (member group-name allowed :test 'eq)
              (defdoc-debug "Dropping unused group ~s~%" group-name)
              (when (gethash group-name group-hash)
                (warn "Group ~s nonempty, but excluded from output"
                      group-name))
              (remhash group-name group-hash))))

        ;; Extract the actual list of group names.
        (let ((group-name-list (loop for group-name being the hash-keys
                                   of group-hash collect group-name)))
          (defdoc-debug "Group-name-list [1] ~s~%" group-name-list)

          ;; Should we sort the groups?
          (cond

           ;; If we have an explicit sorter, apply it.
           ;;
           ;; FILL IN

           ;; If we have a group list specified, mimic it.
           (groups-supp-p
            (setf group-name-list
              (loop for g in allowed
                  if (member g group-name-list)
                  collect g)))

           ;; Otherwise we just leave it alone.
           )

          (defdoc-debug "Group-name-list [2] ~s~%" group-name-list)

          (loop for group in group-name-list
              collect
              (let ((spec-list (gethash group group-hash))
                    (item-order (gethash group group-order-spec)))
                (when item-order
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
                                  (t nil))))))))
                (defdoc-debug " --> group ~s ~s~%" group spec-list)

                (make-instance 'grouped-subframework
                  :labeldef grouping-label-def :group group
                  :spec-list spec-list))))))))

(defun default-compile-grouper (label-name groups groups-supp-p
                                           package package-supp-p forms)
  (let ((adjusted-groups
         (cond
           (package-supp-p
            (loop for g in groups
                  collect (cond
                            ((and (symbolp g)
                                  (eq *package* (symbol-package g)))
                             (intern (symbol-name g) package))
                            ((and (consp g)
                                  (car g)
                                  (symbolp (car g))
                                  (not (keywordp (car g)))
                                  (eq *package* (symbol-package (car g))))
                             (cons (intern (symbol-name (car g)) package)
                                   (cdr g)))
                            (t g))))
           (t groups))))
    `(get-specs-for-output-framework
      (make-instance 'grouped-output-framework
        :group-specs ,adjusted-groups
        :group-specs-supp-p ,groups-supp-p
        :base-label ',label-name
        :generating-thunk (named-function default-generating-thunk
                            (lambda ()
                              ,(case (length forms)
                                 ((1) (car forms))
                                 (otherwise
                                  `(append ,@(loop for form in forms
                                                 collect (eval form)))))))))))

(defvar *grouping-compiler* #'default-compile-grouper)

(defmacro collect-groups-by-label ((label-name &key
                                               (package nil package-supp-p)
                                               (groups nil groups-supp-p)
                                               &allow-other-keys) &body forms)
  (let ()
    `(values ',(funcall *grouping-compiler* label-name
                        groups groups-supp-p
                        package package-supp-p
                        forms)
             nil)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defclass grouped-subframework ()
     ((labeldef :initarg :labeldef :reader labeldef)
      (group :initarg :group :reader group)
      (spec-list :initarg :spec-list :reader spec-list)))

(defmethod format-doc (stream style (group-inst grouped-subframework))

  (with-accessors ((labeldef labeldef)
                   (group group)
                   (spec-list spec-list)) group-inst
    (format-output-pregroup style stream group-inst labeldef group)
    (let ((*sectioning-level* (+ 1 *sectioning-level*)))
      (loop for (spec . other-specs) on spec-list do
            (format-output-prespec style stream group-inst spec)
            (format-doc stream style spec)
            (format-output-postspec style stream group-inst spec)
            (when other-specs
              (format-output-spec-sep style stream group-inst
                                      spec (car other-specs)))))
    (format-output-postgroup style stream group-inst group)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
          ((and name-or-spec (listp name-or-spec)
                (car name-or-spec)
                (symbolp (car name-or-spec))
                (not (keywordp (car name-or-spec))))
           (setf name (car name-or-spec) options (cdr name-or-spec)))

          ;; There's output set options, but no name.
          ((and name-or-spec (listp name-or-spec)
                (car name-or-spec)
                (keywordp (car name-or-spec)))
           (setf name (gensym) options name-or-spec)))
    (values `(list (make-instance ',name))
            (apply #'get-output-framework-forms name forms options))))

(defun collect-named-output (name)
  (values `(list (make-instance ',name)) nil))
