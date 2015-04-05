(defpackage :transit-cl
  (:use :cl :alexandria :local-time :gmap)
  (:import-from :yason #:true #:false)
  (:import-from :fset #:seq #:do-map #:convert #:image #:empty-seq #:push-last
                #:domain #:with-first #:empty-map #:lookup #:@ #:empty?
                #:iterator #:do-seq #:equal? #:size #:arb)
  (:shadowing-import-from :fset #:map #:set #:notany #:some #:every #:includef)
  (:export #:vec
           #:make-quoted-value
           #:make-bytes
           #:make-decimal
           #:make-point
           #:transit-write
           #:transit-write-string
           #:transit-read
           #:roundtrip
           #:verbosely
           #:true
           #:false)
  (:documentation "Cognitect's transit-format for Common Lisp."))
