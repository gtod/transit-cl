(asdf:defsystem transit-cl-test
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.0"
  :license "MIT"
  :description "Tests for transit-cl."
  :depends-on (#:transit-cl #:fiveam #:alexandria #:named-readtables #:cl-ppcre)
  :components 
  ((:module "t"
    :serial t
    :components ((:file "package")
                 (:file "exemplars")
                 (:file "transit")
                 (:file "implementation")))))

(defmethod perform ((o test-op) (c (eql (find-system :transit-cl-test))))
  (declare (ignore o c))
  (flet ((run-tests (package label)
           (funcall (find-symbol (string :run!) :5am)
                    (find-symbol (format nil "~A" label) package))))
    (run-tests :transit-cl :implementation)
    (run-tests :transit-cl-test :transit-cl)
    (run-tests :transit-cl-exemplars :exemplars)))
