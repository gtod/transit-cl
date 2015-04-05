(in-package :transit-cl)

(defpackage :transit-cl-test
  (:use :cl :transit-cl :fiveam :local-time)
  (:import-from :fset #:equal? #:size #:empty-map)
  (:shadowing-import-from :fset #:map #:set))

(defpackage :transit-cl-exemplars
  (:use :cl :transit-cl :fiveam :named-readtables :alexandria :cl-ppcre
        :local-time :uuid)
  (:export #:*exemplars-dir*))
