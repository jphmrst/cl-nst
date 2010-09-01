(in-package :defdoc)

(defun deconstruct-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec nil))
   ((listp name-or-spec)
    (destructuring-bind (name &key method) name-or-spec
      (values name method)))))