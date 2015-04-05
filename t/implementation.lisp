(in-package :transit-cl)

(5am:def-suite implementation)
(5am:in-suite implementation)

(5am:test composite-tagged-p
   (5am:finishes (composite-tagged-p #())))

;; Fixes https://github.com/cognitect/transit-format/issues/21
(5am:test recursive-key-p
  (5am:is (equal? (map ("cached" 0) ('explain "cached"))
                  (transit-read "[\"^ \",[\"~#'\",\"cached\"],0,\"~$EXPLAIN\",\"^0\"]"))))
