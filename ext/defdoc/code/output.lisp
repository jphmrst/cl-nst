;;; File output.lisp
;;;
;;; This file is part of the DefDoc documentation support package.
;;;
;;; Copyright (c) 2010 Smart Information Flow Technologies.
;;; Written by John Maraist.
;;;
;;; NST is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; NST is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with NST.  If not, see
;;; <http://www.gnu.org/licenses/>.
(in-package :defdoc)

(defvar +output-frameworks+ (make-hash-table :test 'eq))

(defgeneric get-compiled-output-framework (package name forms))
(defvar *output-compiler* #'get-compiled-output-framework)

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

(defmacro def-output-framework (name &body body)
  (let ((p (gensym))
        (spec (gensym)))
    ;; (format t "** X ** ~s~%" body)
    `(let* ((,p (symbol-package ',name))
            (,spec (funcall *output-compiler* ,p ',name ',body)))
       (setf (gethash ',name +output-frameworks+) ,spec)
       (when (and (standard-output-framework-groups-supp-p ,spec)
                  (not (standard-output-framework-grouping-label-supp-p ,spec)))
         (warn 'option-without-required-option
               :given :groups :missing :grouping-label
               :def-type 'def-output-framework :name ',name))
       ',name)))

(defun get-output-framework (name)
  (gethash name +output-frameworks+))

(defun get-output-frameworks-collector (names)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for name-or-spec in names do
        (with-name-and-filters (name filters name-or-spec)
          (let ((spec (get-output-framework name)))
            (when (funcall (get-collector-filter-predicate filters) spec)
              (setf (gethash spec result) t)))))
      result))

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
     ((collector :initform #'(lambda (&optional (result
                                                 (make-hash-table :test 'eq)))
                               result)
                 :accessor standard-output-framework-collector)
      (grouping-label :reader standard-output-framework-grouping-label)
      (groups :reader standard-output-framework-groups)
      (default-group :reader standard-output-framework-default-group)
      (doc-title  :reader standard-output-framework-doc-title)
      (doc-author :reader standard-output-framework-doc-author)))

(def-slot-supp-predicates standard-output-framework
    ((standard-output-framework-grouping-label-supp-p grouping-label)
     (standard-output-framework-groups-supp-p         groups)
     (standard-output-framework-doc-title-supp-p      doc-title)
     (standard-output-framework-doc-author-supp-p     doc-author)
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
                              #'(lambda (x y)
                                  (let ((xp (position (docspec-self x)
                                                      item-order :test 'eq))
                                        (yp (position (docspec-self y)
                                                      item-order :test 'eq)))
                                    (cond
                                     ((and (numberp xp) (numberp yp))
                                      (< xp yp))
                                     ((and (numberp xp) (null yp)) t)
                                     (t nil))))))))

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
  #'(lambda (stream output)
      (pprint-logical-block (stream '(1))
        (format stream "[ standard-output-framework ~s"
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
        (format stream " ]"))))

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
     (error "Unrecognized form (~a~{ ~s~}) in def-output-framework %s"
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
                                      #'(lambda (spec)
                                          (eql (label-value spec label)
                                               value))))))

  (:method ((form-head (eql :grouping-label))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (label) form-args
       (set-single-assign-slot output-framework 'grouping-label label)))

  (:method ((form-head (eql :groups))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (set-single-assign-slot output-framework 'groups form-args))

  (:method ((form-head (eql :default-group))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (set-single-assign-slot output-framework 'default-group form-args))

  (:method ((form-head (eql :doc-title))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (title-string) form-args
       (set-single-assign-slot output-framework 'doc-title title-string)))

  (:method ((form-head (eql :doc-author))
            (output-framework standard-output-framework) package form-args)
     (declare (ignore package))
     (destructuring-bind (author-string) form-args
       (set-single-assign-slot output-framework 'doc-author author-string)))

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
  (:method ((output-framework standard-output-framework)
            package form-head form-args)
     (default-standard-output-framework-form-processor
         form-head output-framework package form-args)))

(defmethod get-compiled-output-framework (package name forms)
  (let* ((use-class (get-output-framework-class package name forms))
         (result (make-instance use-class :name name)))
    (loop for form in forms do
      (process-standard-output-framework-form result package
                                                     (car form) (cdr form)))
    result))
