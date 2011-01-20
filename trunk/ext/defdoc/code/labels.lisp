;;; File package.lisp
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

(defclass labeled ()
     ((label-values :initform (make-hash-table :test 'eq)
                    :reader label-values)))

(defgeneric label-value (labeled name)
  (:method ((labeled labeled) name)
     (gethash name (label-values labeled))))

(define-setf-expander label-value (labeled-object label-name)
  (let ((store (gensym)))
    (values nil
            nil
            `(,store)
            `(setf (gethash ,label-name (label-values ,labeled-object))
                   (get-label-symbol-value-translation ,labeled-object
                                                       (get-labeldef
                                                        ',label-name)
                                                       ,store))
            `(label-value ,labeled-object ,label-name))))

(defgeneric set-label-value (labeled name value)
  (:method ((labeled labeled) name value)
     (setf (gethash name (label-values labeled)) value)))

(defgeneric get-label-symbol-value-translation (labeled label symbol)
  (:method (labeled label symbol)
     (declare (ignore labeled label))
     symbol))

;;; -----------------------------------------------------------------

(defvar +label-defs+ (make-hash-table :test 'eq))

(defgeneric get-compiled-labeldef (package name options forms))
(defvar *label-compiler* #'get-compiled-labeldef)

(defmacro def-property-label (name options &body body)
  (let ((p (gensym))
        (spec (gensym)))
    `(let* ((,p (symbol-package ',name))
            (,spec (funcall *label-compiler* ,p ',name ',options ',body)))
        (setf (gethash ',name +label-defs+) ,spec)
        ',name)))

(defun get-labeldef (name)
  (gethash name +label-defs+))

;;; -----------------------------------------------------------------

(defvar *label-class* 'standard-doc-label)
(defgeneric get-label-class (package name options forms)
  (:method (package name options forms)
     (declare (ignore package name forms))
     (destructuring-bind (&key (class *label-class*) &allow-other-keys)
         options
       class)))

(defclass doc-label ()
     ((label-name :initarg :name :reader doc-label-name)))

(defclass standard-doc-label (doc-label)
     ((default-subsort :accessor standard-label-default-subsort)))

(defgeneric has-subsort-label (label-def)
  (:method (label-def) (and (typep label-def 'standard-doc-label)
                            (slot-boundp label-def 'default-subsort))))

(defmethod get-compiled-labeldef (package name options forms)
  (let* ((use-class (get-label-class package name options forms))
         (result (make-instance use-class :name name)))
    (loop for form in forms do
          (let ((hd (car form))
                (tl (cdr form)))
            (process-standard-labeldef-form result package hd tl)))
    result))

(defgeneric process-standard-labeldef-form (labeldef package
                                                     form-head form-args)
  (:method ((labeldef standard-doc-label) package form-head form-args)
     (declare (ignore package))
     (case form-head
       ((:default-subsort)
        (destructuring-bind (subsort) form-args
          (setf (slot-value labeldef 'default-subsort) subsort)))
       (otherwise (error 'unrecognized-deflabel-form
                         :head form-head :args form-args
                         :label-name (doc-label-name labeldef))))))

;;; -----------------------------------------------------------------

(defmacro def-label-config ((&key (label nil label-supp-p)
                                  (style t)
                                  (output-framework t)
                                  (package nil package-supp-p)
                                  &allow-other-keys)
                            &body forms)
  (let ((label-def t)
        (values-package
         (cond
           (package-supp-p (find-package package))
           (label-supp-p (symbol-package label))
           (t nil))))
    (when label-supp-p (setf label-def (get-labeldef label)))
    (unless label-def (error "No such label ~s" label))
    `(progn
       ,@(loop for (value . value-keyargs) in forms
               for use-value = (cond
                                 (values-package (intern (symbol-name value)
                                                         values-package))
                                 (t value))
               append (apply #'get-compiled-label-config
                             label-def use-value style output-framework
                             value-keyargs))
       t)))

(defgeneric get-compiled-label-config (label-def value style output
                                                 &key &allow-other-keys)
  (:method-combination append)
  (:method append (label-def value style output &key
                             (title nil title-supp-p)
                             (order nil order-supp-p)
                             &allow-other-keys)
    (let* ((ignores ())
           (l-spec (cond ((eq label-def t) (let ((id (gensym)))
                                             (push id ignores)
                                             id))
                         (t `(,(gensym) ,(type-of label-def)))))
           (s-spec (cond ((eq style t) (let ((id (gensym)))
                                         (push id ignores)
                                         id))
                         (t `(,(gensym) ,style))))
           (o-spec (cond ((eq output t) (let ((id (gensym)))
                                          (push id ignores)
                                          id))
                         (t `(,(gensym) ,output))))
           (v-spec `(,(gensym) (eql ',value))))
      (declare (ignore s-spec))
      `(,@(when title-supp-p
            (addressed-labelconfig-keyarg :title)
            `((defmethod get-label-section-title (,l-spec ,v-spec ,o-spec)
                ,@(when ignores `((declare (ignore ,@ignores))))
                (compile-element *package* nil ',title))
              (defmethod get-label-section-title-supp-p
                  (,l-spec ,v-spec ,o-spec)
                ,@(when ignores `((declare (ignore ,@ignores))))
                t)))
        ,@(when order-supp-p
            (addressed-labelconfig-keyarg :order)
            `((defmethod get-label-section-order (,l-spec ,v-spec ,o-spec)
                ,@(when ignores `((declare (ignore ,@ignores))))
                ',order)
              (defmethod get-label-section-order-supp-p
                  (,l-spec ,v-spec ,o-spec)
                ,@(when ignores `((declare (ignore ,@ignores))))
                t))))))
  (:method :around (label-def value style output
                              &rest key-args &key &allow-other-keys)
           (declare (ignore style output))
           (let ((label-config-keys-to-address (make-hash-table :test 'eq)))
             (declare (special label-config-keys-to-address))

             ;; Set up the hash table of keys to address.
             (loop while key-args do
               (let ((key (pop key-args)))
                 (pop key-args)
                 (setf (gethash key label-config-keys-to-address) t)))

             ;; Process keys.
             (prog1 (call-next-method)

               ;; Make sure we've addressed all keyword arguments.
               (unless (eql (hash-table-count label-config-keys-to-address) 0)
                 (warn "Label ~s (argument ~s) configuration keyword argument~:p ignored:~{ ~s~}"
                       (doc-label-name label-def) value
                       (loop for key being the hash-keys
                           of label-config-keys-to-address
                           collect key)))))))

(defun addressed-labelconfig-keyarg (key)
  (declare (special label-config-keys-to-address))
  (remhash key label-config-keys-to-address))

(defgeneric get-label-section-title (label value output))
(defgeneric get-label-section-order (label value output))

(defmacro def-generic-fn-default-nil (name params)
  `(defgeneric ,name ,params
     (:method ,params (declare (ignore ,@params)) nil)))

(def-generic-fn-default-nil get-label-section-title-supp-p (label value output))
(def-generic-fn-default-nil get-label-section-order-supp-p (label value output))
