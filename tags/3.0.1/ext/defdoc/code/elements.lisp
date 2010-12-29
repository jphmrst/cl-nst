(in-package :defdoc)

(def-element :plain (standard-plain-text :class standard-doc-element
                                         :args (text))
    ((text :initarg :text :reader text-element-text))
  (make-instance 'standard-plain-text :text text))
(set-pprint-dispatch 'standard-plain-text
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(text) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(def-element :latex (standard-latex :class standard-doc-element
                                    :args (text))
     ((latex :initarg :latex :reader latex-element-latex))
  (make-instance 'standard-latex :latex text))
(set-pprint-dispatch 'standard-latex
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(latex) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(def-element :paragraphs (standard-paragraph-list :class standard-doc-element
                                                  :package package :spec spec
                                                  :arg-list args)
    ((paragraphs :initarg :paragraphs :reader paragraphlist-element-items))
  (make-instance 'standard-paragraph-list
    :paragraphs (mapcar #'(lambda (x)
                            (compile-element package spec x))
                        args)))
(set-pprint-dispatch 'standard-paragraph-list
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(paragraphs) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(def-element :seq (standard-sequence :class standard-doc-element
                                     :package package :spec spec
                                     :arg-list args)
     ((elements :initarg :elements :reader sequence-element-items))
  (make-instance 'standard-sequence
    :elements (mapcar #'(lambda (x)
                          (compile-element package spec x))
                      args)))
(set-pprint-dispatch 'standard-sequence
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(elements) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(def-element :code (standard-code :class standard-doc-element :args (string))
    ((code :initarg :code :reader code-element-string))
  (make-instance 'standard-code :code string))
(set-pprint-dispatch 'standard-code
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(code) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(defclass standard-simple-list-environment (standard-doc-element)
    ((specs :initarg :specs :reader list-element-specs)
     (options :initarg :options :reader list-element-options)
     (env-tag :initarg :env-tag :reader list-element-env-tag)))
(set-pprint-dispatch 'standard-simple-list-environment
  #'(lambda (stream spec)
      (pprint-logical-block (stream '(1))
        (format stream "[ ~a" (type-of spec))
        (loop for slot in '(specs options env-tag) do
          (cond
           ((slot-boundp spec slot)
            (format stream "~:@_  - ~a ~w" slot (slot-value spec slot)))
           (t (format stream "~:@_  - no ~a" slot))))
        (format stream " ]"))))

(def-element :itemize (standard-itemize
                       :class standard-simple-list-environment
                       :package package :spec spec
                       :args (options &rest items)) ()
  (make-instance 'standard-itemize
    :options options
    :specs (mapcar #'(lambda (x) (compile-element package spec x)) items)
    :env-tag "itemize"))

(def-element :enumerate (standard-enumerate
                         :class standard-simple-list-environment
                         :package package :spec spec
                         :args (options &rest items)) ()
  (make-instance 'standard-enumerate
    :options options
    :specs (mapcar #'(lambda (x) (compile-element package spec x)) items)
    :env-tag "enumerate"))
