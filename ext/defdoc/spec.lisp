(in-package :defdoc)

(defvar *spec-class* 'standard-doc-spec)
(defgeneric get-spec-class (package name forms)
  (:method (package name forms)
     (declare (ignore package name forms))
     *spec-class*))

(defclass doc-spec ()
     ((self :initarg :self :reader docspec-self)
      (target-type :initarg :target-type :reader docspec-target-type)
      (tags :initarg :tags :accessor docspec-tags)
      (properties :initform (make-hash-table :test 'eq)
                  :reader docspec-properties)))

(defun get-docspec-property (spec name)
  (gethash name (docspec-properties spec)))

(defun set-docspec-property (spec name value)
  (gethash name (docspec-properties spec) value))

(defclass standard-doc-spec (doc-spec)
     ((descriptive :initarg :descriptive :accessor docspec-descriptive)
      (intro  :initarg :intro  :accessor docspec-intro)
      (short  :initarg :short  :accessor docspec-short)
      (full   :initarg :full   :accessor docspec-full)
      (params :initarg :params :accessor docspec-params)
      (callspecs :initarg :callspecs :initform nil :accessor docspec-callspecs)
      (deprecated :initarg :deprecated :accessor docspec-deprecated)))

(defmacro with-possibly-unbound-slotaccessors (specs inst &body body)
  (let ((results body)
        (o (gensym)))
    (loop for (var bound accessor slot) in specs do
      (setf results `((let ((,var nil) (,bound nil))
                        (when (slot-boundp ,o ',slot)
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
  `(let ((,self (docspec-self ,instance))
         (,callspec (docspec-callspecs ,instance)))
     (with-possibly-unbound-slotaccessors
         ((,intro  ,intro-supp-p  docspec-intro  intro)
          (,params ,params-supp-p docspec-params params)
          (,short  ,short-supp-p  docspec-short  short)
          (,full   ,full-supp-p   docspec-full   full))
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
         ((:deprecated)  (setf (docspec-deprecated spec) (car form-args)))
         ((:descriptive) (setf (docspec-descriptive spec) (car form-args)))
         ((:tags) (setf (docspec-tags spec) form-args))
         ((:params)
          (setf (docspec-params spec)
                (loop for (id forms) in form-args
                  collect (list id (compile-element package spec forms)))))
         ((:intro)       (setting-accessor docspec-intro))
         ((:short)       (setting-accessor docspec-short))
         ((:full)        (setting-accessor docspec-full))
         ((:callspec) (setf (docspec-callspecs spec)
                            (mapcar #'(lambda (x)
                                        (get-compiled-callspec package spec x))
                                    form-args)))
         ((:properties) (loop for (name value) in form-args do
                          (set-docspec-property spec name value)))
         (otherwise
          (error "Unrecognized form (~s~{ ~s~}) in docspec body for ~s"
                 form-head form-args target-name))))))
