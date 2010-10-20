(in-package :defdoc)

(defvar *spec-class* 'standard-doc-spec)
(defgeneric get-spec-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *spec-class*))

(defclass standard-doc-spec ()
     ((self :initarg :self :accessor self)
      (descriptive :initarg :descriptive :accessor descriptive)
      (tags   :initarg :tags   :accessor tags)
      (intro  :initarg :intro  :accessor intro)
      (short  :initarg :short  :accessor short)
      (full   :initarg :full   :accessor full)
      (params :initarg :params :accessor params)
      (callspecs :initarg :callspecs :initform nil :accessor callspecs)
      (deprecated :initarg :deprecated :accessor deprecated)))

(defun compile-spec (name target-type spec-args forms)
  (declare (ignore spec-args))
  (get-compiled-spec (symbol-package name) target-type name forms))

(defgeneric get-compiled-spec (package target-type name forms)
  (:method (package target-type name forms)
     (let* ((use-class (get-spec-class package name forms))
            (result (make-instance use-class :self name)))
       (loop for form in forms do
         (let ((hd (car form))
               (tl (cdr form)))
           (process-standard-specdef-form package target-type
                                          hd name tl result)))
       result)))

(defgeneric process-standard-specdef-form (package target-type form-head
                                           target-name form-args spec)
  (:method (package target-type form-head target-name form-args spec)
     (declare (ignore target-type))
     (macrolet ((setting-accessor (acc)
                  `(setf (,acc spec)
                         (compile-element package spec form-args))))
       (case form-head
         ((:deprecated) (setf (deprecated spec) (car form-args)))
         ((:tags) (setf (tags spec) form-args))
         ((:params) (setf (params spec)
                          (loop for (id forms) in form-args
                                collect (list id
                                              (compile-element package
                                                      spec forms)))))
         ((:descriptive) (setting-accessor descriptive))
         ((:intro)       (setting-accessor intro))
         ((:short)       (setting-accessor short))
         ((:full)        (setting-accessor full))
         ((:callspec) (setf (callspecs spec)
                            (mapcar #'(lambda (x)
                                        (get-compiled-callspec package spec x))
                                    form-args)))
         (otherwise
          (error "Unrecognized form (~a~{ ~a~}) in docspec body for ~s"
                 form-head form-args target-name))))))
