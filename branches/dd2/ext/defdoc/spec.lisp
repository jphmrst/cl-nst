(in-package :defdoc)

(defvar *spec-class* 'standard-doc-spec)
(defgeneric get-spec-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *spec-class*))

(defclass doc-spec ()
     ((tags :initarg :tags :accessor tags)
      (target-type :initarg :target-type :reader target-type)))

(defclass standard-doc-spec (doc-spec)
     ((self :initarg :self :accessor self)
      (descriptive :initarg :descriptive :accessor descriptive)
      (intro  :initarg :intro  :accessor intro)
      (short  :initarg :short  :accessor short)
      (full   :initarg :full   :accessor full)
      (params :initarg :params :accessor params)
      (callspecs :initarg :callspecs :initform nil :accessor callspecs)
      (deprecated :initarg :deprecated :accessor deprecated)))

(defmacro with-possibly-unbound-slotaccessors (specs inst &body body)
  (let ((results body)
        (o (gensym)))
    (loop for (var accessor bound) in specs do
      (setf results `((let (,var ,bound)
                        (when (slot-boundp ,o ',accessor)
                          (setf ,var (,accessor ,o) ,bound t))
                        ,@results))))
    (cond
      (specs `(let ((,o ,inst)) ,@results))
      ((null results) nil)
      ((eql 1 (length results)) (car results))
      (t `(progn ,@results)))))

(defmacro with-unpacked-standard-spec ((self intro intro-supp-p
                                             params params-supp-p
                                             short short-supp-p
                                             full full-supp-p
                                             callspec)
                                       instance &body forms)
  `(let ((,self (self ,instance)) (,callspec (callspecs ,instance)))
     (with-possibly-unbound-slotaccessors ((,intro  intro  ,intro-supp-p)
                                           (,params params ,params-supp-p)
                                           (,short  short  ,short-supp-p)
                                           (,full   full   ,full-supp-p))
         ,instance
       ,@forms)))

(set-pprint-dispatch 'standard-doc-spec
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ standard-doc-spec")
        (loop for slot in '(tags
                            target-type self descriptive intro short full
                            params callspecs deprecated)
              do
           (cond
             ((slot-boundp spec slot)
              (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
             (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(defun compile-spec (name target-type spec-args forms)
  (declare (ignore spec-args))
  (get-compiled-spec (symbol-package name) target-type name forms))

(defgeneric get-compiled-spec (package target-type name forms)
  (:method (package target-type name forms)
     (let* ((use-class (get-spec-class package name forms))
            (result (make-instance use-class
                      :self name :target-type target-type)))
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
         ((:deprecated)  (setf (deprecated spec) (car form-args)))
         ((:descriptive) (setf (descriptive spec) (car form-args)))
         ((:tags) (setf (tags spec) form-args))
         ((:params) (setf (params spec)
                          (loop for (id forms) in form-args
                                collect (list id
                                              (compile-element package
                                                      spec forms)))))
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
