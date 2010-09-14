(in-package :defdoc)

(defmacro def-documentation (name-or-spec &key
                                          (intro nil intro-supp-p)
                                          (params nil params-supp-p)
                                          (short nil short-supp-p)
                                          (full nil full-supp-p)
                                          (callspec nil callspec-supp-p))
  "Doc doc doc"
  (multiple-value-bind (name spec-type spec-args)
      (deconstruct-spec name-or-spec)
    (when params-supp-p
      (loop for name-spec in params do
        (let ((this-spec (cadr name-spec)))
          (when (stringp this-spec)
            (setf (cadr name-spec) `(:plain ,this-spec))))))
    (when (and short-supp-p (stringp short)) (setf short `(:plain ,short)))
    (when (and intro-supp-p (stringp intro)) (setf intro `(:plain ,intro)))
    (when (and  full-supp-p (stringp full))  (setf full  `(:plain ,full)))

    (let ((actual-spec `(:spec :spec-type ,spec-type
                               :spec-args ,spec-args
                               ,@(when intro-supp-p    `(:intro ,intro))
                               ,@(when params-supp-p   `(:params ,params))
                               ,@(when short-supp-p    `(:short ,short))
                               ,@(when full-supp-p     `(:full ,full))
                               ,@(when callspec-supp-p `(:callspec ,callspec))))
          (spec (gensym "spec")))
      `(let ((,spec ',actual-spec))
         (setf (get-doc-spec ',name ',spec-type) ,spec)
         (funcall (get-doctype-lisp-installer ',spec-type) ',name ,spec)))))

