(in-package :transit-cl)

(def-suite :implementation :in :transit-cl)
(in-suite :implementation)

(5am:test composite-tagged-p
   (5am:finishes (composite-tagged-p #())))
