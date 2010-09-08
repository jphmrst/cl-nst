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

(defun defdoc-spec (out-type spec)
  (destructuring-bind (in-type &rest spec-args) spec
    (defdoc-spec-to out-type in-type spec-args)))

(def-spec-format :plain (string)
  (:latex (let ((was-space t))
            (loop for char across string
                append (let ((next-space nil))
                         (prog1
                             (case char
                               (#\\ (coerce "$\\backslash$" 'list))
                               ((#\$ #\% #\# #\& #\_) (list #\\ char))
                               ((#\~ #\^) (list #\\ char #\{ #\}))
                               ((#\")
                                (cond
                                 (was-space (list #\` #\`))
                                 (t (list #\' #\'))))
                               ((#\space #\tab)
                                (setf next-space t) (list #\space))
                               (otherwise (list char)))
                           (setf was-space next-space)))
                into characters
                finally (return `(:latex ,(coerce characters 'string)))))))

(def-spec-format :latex (string)
  (:plain `(:plain ,string)))
