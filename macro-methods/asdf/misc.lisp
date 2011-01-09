
(defclass nst-file (cl-source-file) ())
(defmethod perform ((o compile-op) (c nst-file)) nil)
(defmethod operation-done-p ((op compile-op) (file nst-file)) nil)
(defmethod input-files ((op load-op) (file nst-file))
  (list (component-pathname file)))
(defmethod output-files ((o compile-op) (c nst-file)) nil)
