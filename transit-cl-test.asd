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
