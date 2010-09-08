(in-package :defdoc)

(defmacro def-documentation (name-or-spec &key (intro nil intro-supp-p)
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

    (let ((short-spec (cond
                        (short-supp-p short)
                        (intro-supp-p intro)
                        (full-supp-p full)
                        (t nil))))
      (case spec-type
        (:fn `(progn
                (setf (documentation ',name 'function)
                      (defdoc-spec :plain ',short-spec))))
        (:macro `(progn
                   (setf (documentation ',name 'compiler-macro)
                         (defdoc-spec :plain ',short-spec))))
        (:setf `(progn
                  (setf (documentation ',name 'setf)
                        (defdoc-spec :plain ',short-spec))))
        (:type `(progn
                  (setf (documentation ',name 'type)
                        (defdoc-spec :plain ',short-spec))))
        (:structure `(progn
                       (setf (documentation ',name 'structure)
                             (defdoc-spec :plain ',short-spec))))
        (:package `(progn
                     (setf (documentation ,(find-package name) t)
                           (defdoc-spec :plain ',short-spec))))
        (:method-combination `(progn
                                (setf (documentation ',name 'method-combination)
                                      (defdoc-spec :plain ',short-spec))))
        (:var `(progn
                 (setf (documentation ',name 'variable)
                       (defdoc-spec :plain ',short-spec))))
        (:type `(progn
                   (setf (documentation ',name 'type)
                         (defdoc-spec :plain ',short-spec))))
        (:method nil)))))

