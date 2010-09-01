(in-package :defdoc)

(defmacro def-documentation (name-or-spec &key (intro nil intro-supp-p)
                                          (params nil params-supp-p)
                                          (short nil short-supp-p)
                                          (full nil full-supp-p))
  "Doc doc doc"
   (multiple-value-bind (name method-spec) (deconstruct-spec name-or-spec)
     `(let (doc-symbol)

        (cond
          ((macro-function ',name)
           (setf doc-symbol 'compiler-macro))
          ((block check-fn
             (handler-bind ((undefined-function
                             #'(lambda (e) (return-from check-fn nil))))
               (symbol-function ',name)))
           (setf doc-symbol 'function)))

        (let ((plain-string ,(cond
                               (intro-supp-p
                                (cond
                                  ((stringp intro) intro)
                                  ((symbolp intro) (symbol-name intro))
                                  (t "")
                                  ))
                               (t ""))))
          )
        ,@(cond
           (intro-supp-p `((setf (documentation ',name) ,intro)))
           )
        )))

(defgeneric f (d)
  (:documentation "Blah blah"))

(defmethod f ((d symbol))
  "Bleh bleh")

