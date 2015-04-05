(in-package :transit-cl-exemplars)

;;;; Notes

;; Do we care that both false and null/nil map to CL nil, so that #{ nil false }
;; in EDN and transit has only one element...?

;;;; Incomplete, buggy EDN parser which is just good enough for the EDN
;;;; content in exemplars README.md

(defreadtable edn-syntax
  (:merge :standard)
  (:case :preserve)
  (:macro-char #\, #'(lambda (stream char)
                       (declare (ignore stream char))
                       (values)))
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (coerce (read-delimited-list #\] stream) 'vector)))
  (:macro-char #\] #'(lambda (stream char)
                       (declare (ignore stream char))
                       (get-macro-character #\))))
  (:macro-char #\{ #'(lambda (stream char)
                       (declare (ignore char))
                       (transit-cl::map-from-key-value-vector
                        (coerce (read-delimited-list #\} stream) 'vector))))
  (:macro-char #\} #'(lambda (stream char)
                       (declare (ignore stream char))
                       (get-macro-character #\))))
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\I #'(lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read stream)
                                    (parse-rfc3339-timestring (read stream))))
  (:dispatch-macro-char #\# #\U #'(lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read stream)
                                    (make-uuid-from-string (read stream))))
  (:dispatch-macro-char #\# #\< #'(lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (let ((tag (read stream)))
                                      (cond ((string= "URI" tag)
                                             (puri:parse-uri
                                              (coerce (loop for char = (read-char stream)
                                                            until (char= #\> char)
                                                            collect char)
                                                      'string)))
                                            (t (error "Not implemented"))))))
  (:dispatch-macro-char #\# #\{ #'(lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (fset:convert
                                     'fset:set
                                     (read-delimited-list #\} stream)))))

;; This is very heavy handed but effective because if there are
;; unintended consequences in other tests, *those* tests will fail...
(defun edn-common-lisp-replacements (line)
  (macrolet ((r (regex string)
               `(setf line (regex-replace-all ,regex line ,string))))
    (r "(\\s+[0-9-]+)N" "\\1")
    (r "\\bnil\\b" "NIL")
    (r "\\btrue\\b" "T")
    (r "\\bfalse\\b" "NIL")
    (r "\\bNaN\\b" ":NAN")
    (r " -Infinity\\b" " :-INF")
    (r " Infinity\\b" " :INF")))

(defun qualified-edn-line-p (line)
  (and (scan "^     " line)
       (not (scan "TaggedValueImpl" line))))

;;;; Implementation

(defparameter *exemplars-dir* "/home/gtod/quicklisp/local-projects/transit-format/examples/0.8/simple/")

(defun exemplar-file (file)
  (merge-pathnames file *exemplars-dir*))

(defun foreach-line-in-file (file thunk)
  (with-input-from-file (stream file :external-format :utf-8)
    (loop for line = (read-line stream nil)
          while line do (funcall thunk line))))

(defun verbose-file-p (file)
  (scan "verbose" file))

(defun verbose-file (files)
  (car (remove-if-not #'verbose-file-p files)))

(defun transit-file (files)
  (car (remove-if #'verbose-file-p files)))

(defun read-transit-file (file)
  (with-input-from-file (stream (exemplar-file file) :external-format :utf-8)
    (transit-read stream)))

(defun read-edn-object (line)
  (in-readtable edn-syntax)
  (unwind-protect (read-from-string line)
    (in-readtable nil)))

(let (files)
  (defun per-line (line)
    (when (scan "Files:" line)
      (setf files (all-matches-as-strings "\\S+\\.json" line)))
    (when (qualified-edn-line-p line)
      (let ((line (edn-common-lisp-replacements line)))
        (let ((edn-object (read-edn-object line))
              (transit-object (read-transit-file (transit-file files)))
              (verbose-object (read-transit-file (verbose-file files))))
          (5am:is (fset:equal? edn-object transit-object))
          (5am:is (fset:equal? edn-object verbose-object)))))))

;;;; Interface

(5am:test exemplars
  (foreach-line-in-file (exemplar-file #p"README.md") #'per-line))
