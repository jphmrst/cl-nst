
(defdoc:def-output-framework
    (defdoc-manual :title "DefDoc user manual"
      :author "John Maraist")

  (collect-groups-by-label
      (manual-section :package :defdoc
                      :groups '(docspecs outspec doc-gen control targets model
                                label-model elements standard-model output-model
                                plaintext latex deprecated))
    (collect-exported-symbols :defdoc)
    (collect-exported-symbols :defdoc-control-api)))
