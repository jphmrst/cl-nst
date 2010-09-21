(in-package :defdoc)

(defun format-doc (stream style spec &rest keyargs &key &allow-other-keys)
  (destructuring-bind (&key spec-type &allow-other-keys) (cdr spec)
    (apply #'format-docspec stream style spec spec-type keyargs)))

(defgeneric format-docspec (stream style spec type &key &allow-other-keys)
  (:method :around (stream style spec type
                           &rest key-args &key &allow-other-keys)
           (cond ((symbolp style)
                  (apply #'format-docspec
                         stream (make-instance style) spec type key-args))
                 (t (call-next-method))))
  (:method (stream style spec type &key &allow-other-keys)
     (let ((spec-head (car spec))
           (spec-args (cdr spec)))
       (format-docspec-element style type spec-head stream spec-args))))

(defgeneric format-docspec-element (style spec-type spec-head stream spec-args))
