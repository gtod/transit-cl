(asdf:defsystem transit-cl
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.0"
  :license "MIT"
  :description "Common Lisp implementation of the Transit data exchange format."
  :depends-on (#:alexandria
               #:fset
               #:yason
               #:local-time
               #:cl-base64
               #:wu-decimal
               #:uuid
               #:puri)
  :components 
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "all")))))

(defmethod perform ((o test-op) (c (eql (find-system :transit-cl))))
  (declare (ignore o c))
  (load-system :transit-cl-test)
  (test-system :transit-cl-test))
