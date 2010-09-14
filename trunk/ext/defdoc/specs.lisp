(in-package :defdoc)

(defmacro ensure-margins ((&key) &body body)
  `(let ((-text-width- (if (boundp '-text-width-)
                           (symbol-value '-text-width-)
                           78)))
     (declare (special -text-width-))
     ,@body))

(def-spec-format :spec (&key spec-type spec-args
                             intro params short full callspec))
(def-spec-format :plain (string))
(def-spec-format :latex (string))
(def-spec-format :paragraphs (&rest pars))
(def-spec-format :seq (&rest pars))
(def-spec-format :code (string))
(def-spec-format :itemize (options &rest items))
(def-spec-format :enumerate (options &rest items))
;; (def-spec-format :callspec (s-expr))

