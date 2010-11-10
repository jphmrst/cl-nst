(in-package :defdoc)

(defvar +defdocs+ (make-hash-table :test 'eq)
  "Master global hashtable of all documentation specifiers.")

(defun get-doc-hash-of-target-type (type)
  (let ((type-hash (gethash type +defdocs+)))
    (unless type-hash
      (error "No such documentation type ~s" type))
    type-hash))

(defun get-doc-spec (name type)
  (let ((type-hash (get-doc-hash-of-target-type type)))
    (gethash name type-hash)))

(defun get-doc-specs (name)
  (loop for hash being the hash-values of +defdocs+
        for spec = (gethash name hash)
        if spec collect spec))

(define-setf-expander get-doc-spec (name type)
  (let ((store (gensym))
        (type-hash (gensym)))
    (values nil
            nil
            `(,store)
            `(let ((,type-hash (get-doc-hash-of-target-type ,type)))
               (setf (gethash ,name ,type-hash) ,store))
            `(get-doc-spec ,name ,type))))

(defun all-by-defdocs-storage-collector ()
  (loop for subhash being the hash-values of +defdocs+
        append (loop for spec being the hash-values of subhash
                     collect spec)))

(setf (gethash #'all-by-defdocs-storage-collector +all-specs-collectors+) t)

(defun get-target-types-collector (target-types)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for type in target-types do
        (loop for spec being the hash-values of (gethash type +defdocs+) do
          (setf (gethash spec result) t)))
      result))

(defun get-packages-exported-collector (package-names &optional warn-if-undoc)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for package-name in package-names do
        (do-external-symbols (s (find-package package-name))
          (let ((symbol-used nil))
            (loop for target-type-hash being the hash-values of +defdocs+
                  for spec = (gethash s target-type-hash)
                  if spec do (setf (gethash spec result) t symbol-used t))
            (when (and warn-if-undoc (not symbol-used))
              (warn "No specification for ~s" s)))))
      result))

(defun get-packages-symbols-collector (package-names &optional warn-if-undoc)
  #'(lambda (&optional (result (make-hash-table :test 'eq)))
      (loop for package-name in package-names do
        (do-symbols (s (find-package package-name))
          (let ((symbol-used nil))
            (loop for target-type-hash being the hash-values of +defdocs+
                  for spec = (gethash s target-type-hash)
                  if spec do (setf (gethash spec result) t symbol-used t))
            (when (and warn-if-undoc (not symbol-used))
              (warn "No specification for ~s" s)))))
      result))
