(in-package :defdoc)

(defun get-doc-tags (name type)
  (let ((spec (get-doc-spec name type)))
    (tags spec)))

(defgeneric format-tag (style package tag stream)
  (:method (style package tag stream)
     (declare (ignore style package))
     (format t "DFT for: ~a~%" tag)
     (format stream "~a" tag)))

(defgeneric tag-sort (style package tag)
  (:method (style package tag)
     (warn "~@<Using default tag-sort 0 for:~:@_ style ~a~:@_ package ~a~:@_ tag ~:@_Consider using def-doc-tag~:>" style package tag)
     0))

(defmacro def-doc-tag (tag (&key (package nil package-supp-p) (style t))
                       &key formatter (sort 0))
  ;; (format t "~s ~s ~s~%" tag package style)
  (let ((spt-params `((style ,style)
                      ,(cond
                        (package-supp-p
                         `(package (eql (find-package ,package))))
                        (t 'package))
                      (tag (eql ',tag)))))
    `(progn
       (defmethod format-tag (,@spt-params stream)
         (funcall #',formatter style package tag stream))
       (defmethod tag-sort (,@spt-params) ,sort))))
