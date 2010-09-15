(in-package :defdoc)

(defmacro def-documentation (name-or-spec &body body)
  "Doc doc doc"
  (multiple-value-bind (name spec-type spec-args)
      (deconstruct-spec name-or-spec)
    (let ((actual-spec (decode-defdoc-forms spec-type spec-args body))
          (spec (gensym "spec")))
      `(let ((,spec ',actual-spec))
         (setf (get-doc-spec ',name ',spec-type) ,spec)
         (funcall (get-doctype-lisp-installer ',spec-type) ',name ,spec)))))

;;; -----------------------------------------------------------------

(defun deconstruct-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec (guess-spec-type name-or-spec) nil))
   ((listp name-or-spec)
    (destructuring-bind (spec-type name &rest spec-args) name-or-spec
      (values name spec-type spec-args)))))

(defun decode-defdoc-forms (spec-type spec-args forms)
  (let ((intro nil) (intro-supp-p nil)
        (params nil) (params-supp-p nil)
        (short nil) (short-supp-p nil)
        (full nil) (full-supp-p nil)
        (callspec nil) (callspec-supp-p nil))
    (flet ((decode-spec-args (args)
             (cond
              ((not (listp args)) args)
              ((eql 1 (length args)) (car args))
              (t `(:paragraphs ,@args)))))
      (macrolet ((assign-text-spec (tag args var supp-var)
                   `(cond
                     (,supp-var (error "Duplicate intro: ~s" ,args))
                     (t (setf ,supp-var t
                              ,var (decode-spec-args ,args))))))
        ;; Unpack the forms
        (loop for form in forms do
              (destructuring-bind (tag . args) form
                (case tag
                  ((:intro) (assign-text-spec tag args intro intro-supp-p))
                  ((:short) (assign-text-spec tag args short short-supp-p))
                  ((:full)  (assign-text-spec tag args full  full-supp-p))

                  ((:params)
                   (cond
                    (params-supp-p (error "Duplicate param: ~s" args))
                    (t (setf params-supp-p t params args))))

                  ((:callspec)
                   (cond
                    (callspec-supp-p (error "Duplicate callspec: ~s" args))
                    (t (setf callspec-supp-p t callspec args))))

                  (otherwise (error "Unrecognized ~s" form)))))))

    (when params-supp-p
      (loop for name-spec in params do
        (let ((this-spec (cadr name-spec)))
          (when (stringp this-spec)
            (setf (cadr name-spec) `(:plain ,this-spec))))))
    (when (and short-supp-p (stringp short)) (setf short `(:plain ,short)))
    (when (and intro-supp-p (stringp intro)) (setf intro `(:plain ,intro)))
    (when (and  full-supp-p (stringp full))  (setf full  `(:plain ,full)))

    (values `(:spec :spec-type ,spec-type :spec-args ,spec-args
                    ,@(when intro-supp-p    `(:intro ,intro))
                    ,@(when params-supp-p   `(:params ,params))
                    ,@(when short-supp-p    `(:short ,short))
                    ,@(when full-supp-p     `(:full ,full))
                    ,@(when callspec-supp-p `(:callspec ,callspec)))
            intro intro-supp-p
            params params-supp-p
            short short-supp-p
            full full-supp-p
            callspec callspec-supp-p)))
