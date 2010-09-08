(in-package :defdoc-ex)

(defgeneric f (d))
(def-documentation (:fn f)
    :intro "Theoretical implementation of the Ackermann function"
    :params ((d (:latex "As for the $\Gamma$ function argument.")))
    :full "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas quis massa tellus, sodales pulvinar dolor. Cras eu placerat orci. Maecenas eu arcu molestie purus tincidunt semper ut ut lectus. Donec.")

(defmethod f ((d symbol))
  "Bleh bleh")
(def-documentation (:method f (symbol))
    :intro "Tractable implementation of the Ackermann function for symbols."
    :params ((d (:plain "Lorem ipsum dolor sit amet, consectetur.")))
    :full (:latex "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In posuere enim vel arcu luctus mollis. Nunc vitae dui eget quam gravida accumsan et ut nisi. Vivamus posuere, turpis consequat suscipit consequat, orci nisi sollicitudin libero, in lobortis massa tellus et purus. Donec molestie lorem eu nisl accumsan viverra. Nam dictum ipsum sit amet arcu eleifend condimentum. Duis bibendum convallis magna. Aenean eleifend lacus non urna sodales aliquam vulputate est bibendum. Aenean tincidunt posuere blandit. Vestibulum varius arcu sit amet arcu vehicula feugiat. Nullam dignissim lacinia nunc. Aenean pellentesque semper arcu ultrices faucibus. Etiam non enim eget sapien eleifend rutrum elementum ut augue. Proin semper turpis nec magna porta et tempus sapien facilisis. Proin eget lacus elit, ut commodo nunc. Curabitur quis ipsum mauris, eu euismod ligula. Curabitur dolor diam, ultricies vitae sollicitudin eu, tincidunt sit amet sapien. In commodo vehicula bibendum.

 Donec est odio, tincidunt sit amet interdum ut, placerat a tellus. Duis feugiat rutrum nibh, non vulputate diam adipiscing quis. In eu hendrerit leo. Vestibulum risus metus, iaculis in faucibus ut, condimentum quis nisi. Integer in imperdiet turpis. Suspendisse bibendum enim et nunc consequat et laoreet mi auctor. Aenean tristique, quam ac porttitor egestas, eros mauris ultricies mi, porta elementum dui tortor volutpat nulla. Aliquam vestibulum varius dapibus. Praesent nec purus nisi. Donec ipsum massa, malesuada in porta sit amet, suscipit id sapien. Duis condimentum odio cursus ligula tristique pellentesque. Proin ut diam ac urna feugiat scelerisque. Aenean faucibus neque eget metus rhoncus sodales. Aliquam erat volutpat. Sed tellus dui, sollicitudin in scelerisque at, sagittis eu nulla. Morbi scelerisque tempor arcu, in sodales mi facilisis quis. Mauris at arcu quam, ut accumsan nulla."))

(defun g (x y) (+ x y))
(def-documentation (:fun g)
    :intro "Lorem ipsum dolor sit amet."
    :params ((x (:latex "Vestibulum ut diam vel nisi $\nu\epsilon\sigma\tau\iota\beta\upsilon\lambda\upsilon\mu$ hendrerit. Duis auctor."))
             (y (:plain "Morbi pharetra elementum consectetur. Duis ultrices odio.")))
    :full (:latex "Sed at sem mi. In ut nibh ante, non euismod odio. Suspendisse quis ipsum augue, ac porttitor odio. In pharetra molestie consectetur. Donec ut felis enim. Curabitur nisi lectus, suscipit non vulputate in, adipiscing non nulla. Aliquam nulla eros, aliquet id tempus sit amet, sagittis ac nulla. Donec eros est, suscipit sed placerat vitae, commodo tristique purus. Nulla ac neque sed lacus aliquet varius. Donec elementum mauris quis ipsum fermentum eu hendrerit tortor faucibus. Nulla ac lectus sit amet enim commodo eleifend at et eros. Donec quis urna ipsum, at consequat nisi. Nunc in est non augue bibendum volutpat nec ac leo. Sed venenatis eleifend dictum."))

(defvar *hh* 8)
(def-documentation *hh*
  :short "Aliquam erat volutpat. Donec vel."
  :full "Duis sit amet bibendum nisi. Etiam elementum risus eu ante porta dapibus. Curabitur eu urna eget arcu malesuada iaculis nec.")
