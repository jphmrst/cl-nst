(in-package :defdoc)

(def-label nst-manual (spec package)
  (:sort nst-manual-section)
  (:default-subsort nst-manual-subsection)
  )

(def-doc-output-set manual
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

    ;; Labels and values of this set.
    ;;
    ;; (:labels (nst-volume 1))

  (:grouping-label nst-manual)
  (:groups fixtures groups tests criteria)

  ;; Where the contents come from.  These are disjunctive; could
  ;; specify conjunctions one level in as:
  ;;
  (:with-sets manual-criteria)
  (:exported-symbols package)
                                        ; (:target-type (function :package :nst))
                                        ; (:documented-symbols package)
                                        ; (:all-symbols package)
                                        ; (:target-type criterion)

  )

(def-doc-output-set manual-criteria
  (:labels (nst-manual criteria))

  (:grouping-label nst-criteria-group)
  (:groups )

  (:target-type criterion)
  )

(def-documentation (function blah)
    (:labels (nst-manual fixtures)
             (nst-manual-section 0)
             (nst-quickref user-macros)
             (nst-quickref-section 2)))

