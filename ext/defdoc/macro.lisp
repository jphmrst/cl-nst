(in-package :defdoc)

(defmacro def-documentation (name-or-spec &key
                                          (intro nil intro-supp-p)
                                          (params nil params-supp-p)
                                          (short nil short-supp-p)
                                          (full nil full-supp-p))
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

    (let ((spec `(:spec :spec-type ,spec-type
                        :spec-args ,spec-args
                        ,@(when intro-supp-p  `(:intro ,intro))
                        ,@(when params-supp-p `(:params ,params))
                        ,@(when short-supp-p  `(:short ,short))
                        ,@(when full-supp-p   `(:full ,full)))))
      (setf (gethash name (gethash spec-type +defdocs+)) spec)
      (case spec-type
        (:fn `(progn
                (setf (documentation ',name 'function)
                      (spec-to-text ',spec))))
        (:macro `(progn
                   (setf (documentation ',name 'compiler-macro)
                         (spec-to-text ',spec))))
        (:setf `(progn
                  (setf (documentation ',name 'setf)
                        (spec-to-text ',spec))))
        (:type `(progn
                  (setf (documentation ',name 'type)
                        (spec-to-text ',spec))))
        (:structure `(progn
                       (setf (documentation ',name 'structure)
                             (spec-to-text ',spec))))
        (:package `(progn
                     (setf (documentation ,(find-package name) t)
                           (spec-to-text ',spec))))
        (:method-combination `(progn
                                (setf (documentation ',name 'method-combination)
                                      (spec-to-text ',spec))))
        (:var `(progn
                 (setf (documentation ',name 'variable)
                       (spec-to-text ',spec))))
        (:type `(progn
                   (setf (documentation ',name 'type)
                         (spec-to-text ',spec))))
        (:method nil)))))

