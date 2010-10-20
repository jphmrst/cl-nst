(in-package :defdoc)

(def-element :plain (standard-plain-text :class standard-doc-element
                                         :args (text))
    ((text :initarg :text :accessor text))
  (make-instance 'standard-plain-text :text text))

(def-element :latex (standard-latex :class standard-doc-element
                                    :args (text))
     ((latex :initarg :latex :accessor latex))
  (make-instance 'standard-latex :latex text))

(def-element :paragraphs (standard-paragraph-list :class standard-doc-element
                                                  :package package :spec spec
                                                  :arg-list args)
    ((paragraphs :initarg :paragraphs :accessor paragraphs))
  (make-instance 'standard-paragraph-list
    :paragraphs (mapcar #'(lambda (x)
                            (compile-element package spec x))
                        args)))

(def-element :seq (standard-sequence :class standard-doc-element
                                     :package package :spec spec
                                     :arg-list args)
     ((elements :initarg :elements :accessor elements))
  (make-instance 'standard-sequence
    :elements (mapcar #'(lambda (x)
                          (compile-element package spec x))
                      args)))

(def-element :code (standard-code :class standard-doc-element :args (string))
    ((code :initarg :code :accessor code))
  (make-instance 'standard-code :code string))

(def-element :itemize (standard-itemize :class standard-doc-element
                                        :package package :spec spec
                                        :args (options &rest items))
    ((specs :initarg :specs :accessor specs)
     (options :initarg :options :accessor options))
  (make-instance 'standard-itemize
    :options options
    :specs (mapcar #'(lambda (x) (compile-element package spec x)) items)))

(def-element :enumerate (standard-enumerate :class standard-doc-element
                                            :package package :spec spec
                                            :args (options &rest items))
    ((specs :initarg :specs :accessor specs)
     (options :initarg :options :accessor options))
  (make-instance 'standard-enumerate
    :options options
    :specs (mapcar #'(lambda (x) (compile-element package spec x)) items)))
