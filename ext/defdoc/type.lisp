(in-package :defdoc)

(def-doctype function ()
  (:docstring-installer (name spec)
    (setf (documentation name 'function) (spec-to-text spec))))

(def-doctype compiler-macro ()
  (:docstring-installer (name spec)
    (setf (documentation name 'compiler-macro) (spec-to-text spec))))

(def-doctype setf ()
  (:docstring-installer (name spec)
    (setf (documentation name 'setf) (spec-to-text spec))))

(def-doctype type ()
  (:docstring-installer (name spec)
    (setf (documentation name 'type) (spec-to-text spec))))

(def-doctype structure ()
  (:docstring-installer (name spec)
    (setf (documentation name 'structure) (spec-to-text spec))))

(def-doctype package ()
  (:docstring-installer (name spec)
    (setf (documentation (find-package name) t) (spec-to-text spec))))

(def-doctype method-combination ()
  (:docstring-installer (name spec)
    (setf (documentation name 'method-combination) (spec-to-text spec))))

(def-doctype variable ()
  (:docstring-installer (name spec)
    (setf (documentation name 'variable) (spec-to-text spec))))

(def-doctype method ())

