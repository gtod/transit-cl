(in-package :transit-cl)

(use-package :fiveam)

(defpackage :transit-cl-exemplars
  (:use :cl :transit-cl :fiveam :named-readtables :alexandria :cl-ppcre
        :local-time :uuid))
