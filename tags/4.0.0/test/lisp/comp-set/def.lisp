
(defmacro make-create-setslot (class-name)
  (let ((o1 (gensym)) (o2 (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class-name () ((zslot :reader zslot)))
       (let ((,o1 (make-instance ',class-name)))
         (setf (slot-value ,o1 'zslot) 3)
         (let ((,o2 (make-instance ',class-name)))
           (setf (slot-value ,o2 'zslot) (zslot ,o1)))))))