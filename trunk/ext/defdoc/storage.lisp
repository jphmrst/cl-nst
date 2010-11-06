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