(in-package :defdoc)

(defvar *docstring-style* 'standard-docstring-style)

(defun format-doc (stream style spec)
  (let ((target-type (docspec-target-type spec)))
    (format-docspec stream style spec target-type)))

(defgeneric format-docspec (stream style spec type)
  (:method :around (stream style spec type)
     (cond ((symbolp style)
            (format-docspec stream (make-instance style) spec type))
           (t (call-next-method))))
  (:method (stream style spec type)
     (format-docspec-element style type spec stream)))

(defgeneric format-docspec-element (style target-type spec stream))
