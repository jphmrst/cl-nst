(in-package :defdoc)

(def-property-label nst-manual (spec package)
  (:sort nst-manual-section)
  (:default-subsort nst-manual-subsection)
  )

(def-output-framework manual
    ;; Set the style to be associated with this output set.
    ;;
    ;; (:style style-class)

    ;; Labels and values of this set.
    ;;
    ;; (:property-values (nst-volume 1))

  (:grouping-label nst-manual)
  (:groups fixtures groups tests criteria)

  ;; Where the contents come from.  These are disjunctive; could
  ;; specify conjunctions one level.
  ;;
  (:with-output manual-criteria) ; This one NOT covered by select-docspecs
  (:exported-symbols package)
                                        ; (:target-type (function
                                        ;                 (:package :nst)))
                                        ; (:documented-symbols package)
                                        ; (:all-symbols package)
                                        ; (:target-type criterion)

  )

(def-output-framework manual-criteria
  (:property-values (nst-manual criteria))

  (:grouping-label nst-criteria-group)
  (:groups )

  (:target-type criterion)
  )

(def-documentation (function blah)
    (:properties (nst-manual fixtures)
                 (nst-manual-section 0)
                 (nst-quickref user-macros)
                 (nst-quickref-section 2)))

