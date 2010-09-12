(in-package :defdoc)

(defun deconstruct-spec (name-or-spec)
  (cond
   ((symbolp name-or-spec)
    (values name-or-spec (guess-spec-type name-or-spec) nil))
   ((listp name-or-spec)
    (destructuring-bind (spec-type name &rest spec-args) name-or-spec
      (values name spec-type spec-args)))))

(defun guess-spec-type (name)
  (cond
    ((fboundp name) :fn)
    ((boundp name) :var)
    (t (error "Cannot determine name use for documentation of ~s" name))))

(defmacro ensure-margins ((&key) &body body)
  `(let ((-text-width- (if (boundp '-text-width-)
                           (symbol-value '-text-width-)
                           78)))
     (declare (special -text-width-))
     ,@body))

(def-spec-format :plain (string))
(def-spec-format :latex (string))
(def-spec-format :paragraphs (&rest pars))
(def-spec-format :seq (&rest pars))
(def-spec-format :itemize (options &rest items))
(def-spec-format :enumerate (options &rest items))

