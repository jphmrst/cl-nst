(in-package :defdoc)

(defgeneric format-docspec (stream style spec &key &allow-other-keys)
  (:method :around (stream style spec &rest key-args &key &allow-other-keys)
           (cond
             ((symbolp style)
              (apply #'format-docspec
                     stream (make-instance style) spec key-args))
             (t (call-next-method))))
  (:method (stream style spec &key &allow-other-keys)
     (format-docspec-element (car spec) stream style (cdr spec))))

(defgeneric format-docspec-element (spec-head stream style spec-args))
