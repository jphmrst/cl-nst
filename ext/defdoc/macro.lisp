
(defmacro def-documentation (name-or-spec &key (intro nil intro-supp-p)
                                          (params nil params-supp-p)
                                          (full nil full-supp-p))
  (multiple-value-bind (name method-spec) name-or-spec
  `(progn
     ,@(cond
         (intro-supp-p `((setf (documentation ',name) ,intro)))
         )
     )))

