;;;; Transit format in Common Lisp
;;;; All in one file for ease of initial development.

(in-package :transit-cl)

(define-constant +esc+ #\~)
(define-constant +tag-char+ #\#)
(define-constant +escape-chars+ '(#\~ #\^ #\`) :test #'equal)
(define-constant +cached-string-chars+ '(#\: #\$ #\#) :test #'equal)

(defvar *verbose-p* nil
  "Set true to use transit's verbose JSON output mode.")

(defmacro verbosely (form)
  `(let ((*verbose-p* t))
     ,form))

;;;; util

(defun vec (&rest list)
  (make-array (length list) :initial-contents list))

(defun signed-integer-length (integer)
  (1+ (integer-length integer)))

(defun vector-rest (vector)
  (make-array (1- (length vector))
              :displaced-to vector
              :displaced-index-offset 1))



;;;; Although most of the transit types have strong Common Lisp
;;;; support we need to make a few additions...

(deftype octet () '(unsigned-byte 8))

(defmacro def-box-type (name type)
  `(progn
     (defclass ,name ()
       ((value :type ,type
               :initarg :value
               :reader value)))

     (defun ,(symbolicate 'make- name) (value)
       (make-instance ',name :value value))

     (defmethod print-object ((object ,name) stream)
       (print-unreadable-object (object stream :type t)
         (prin1 (slot-value object 'value) stream)))

     (defmethod fset:compare ((a ,name) (b ,name))
       (fset:compare-slots a b 'value))))

;;; octet vectors

(def-box-type octet-vector (vector octet))

(defun make-bytes (list)
  (make-octet-vector
   (make-array (length list) :element-type 'octet :initial-contents list)))

;;; arbitrary decimals

(defparameter *transit-pprint-dispatch* (copy-pprint-dispatch)
  "Our copy of the current pretty print dispatch table.  Used, at least,
for printing arbitrary decimals.")

(let ((*print-pprint-dispatch* *transit-pprint-dispatch*))
  (wu-decimal:enable-decimal-printing-for-ratios))

(def-box-type arbitrary-decimal wu-decimal:decimal)

(defun make-decimal (string)
  (make-arbitrary-decimal (wu-decimal:parse-decimal string)))

(defmethod print-object :around ((object arbitrary-decimal) stream)
  (declare (ignore stream))
  (let ((*print-pprint-dispatch* *transit-pprint-dispatch*))
    (call-next-method)))

;;; local-time

(defparameter *rfc3339-format*
  '((:year 4) #\- (:month 2) #\- (:day 2)
    #\T
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3)
    :gmt-offset-or-z)
  "This is our canonical format for RFC 3339 strings.  The timezone is
always UTC and the granularity is milliseconds.  All times represented
in this format will always have the same length and thus may be sorted
and compared.")

(defgeneric rfc3339-string (date-time)
  (:method ((stamp timestamp))
  "Convert local-time timestamp STAMP to our canonical RFC 3339 UTC
string format."
  (format-timestring nil stamp :format *rfc3339-format* :timezone +utc-zone+))
  (:documentation "Convert DATE-TIME to the an RFC 3339 section 5.6
'date-time' format, a string, as specified by local-time format
*RFC3339-FORMAT*."))

(defun epoch-msec-to-timestamp (msec)
  (multiple-value-bind (sec msec)
      (floor msec 1000)
    (unix-to-timestamp sec :nsec (* (expt 10 6) msec))))

(defun timestamp-to-epoch-msec (stamp)
  (+ (* 1000 (timestamp-to-unix stamp))
     (round (/ (nsec-of stamp) (expt 10 6)))))

(defmethod fset:compare ((a timestamp) (b timestamp))
  (cond ((timestamp< a b) ':less)
        ((timestamp> a b) ':greater)
        (t ':equal)))

;;; uri

(defmethod fset:compare ((a puri:uri) (b puri:uri))
  (if (puri:uri= a b)
      ':equal
      ':unequal))

;;; uuid

(defmethod fset:compare ((a uuid:uuid) (b uuid:uuid))
  (if (uuid:uuid= a b)
      ':equal
      ':unequal))

;;; quoted value

(def-box-type quoted-value t)





;;;; caching

;;; transit-js has a generation optimization we don't have

(define-constant +cache-code-digits+ 44)
(define-constant +base-char-index+ 48)
(define-constant +min-size-cacheable+ 3)
(define-constant +max-cache-entries+ (expt +cache-code-digits+ 2))

(defvar *cache-index* nil
  "Bind to 0 before writing a transit document.")

(defvar *cache-map* nil
  "Bind to an empty Fset map before writing a transit document.")

(defvar *cache-vector* nil
  "Bind to a vector before reading a transit document.")

;;;; Impl

(defun cache-unit-integer (code pos)
  (- (char-code (elt code pos)) +base-char-index+))

(defun cache-code-to-index (code)
  (declare (type string code))
  (ecase (length code)
    (2 (cache-unit-integer code 1))
    (3 (+ (* (cache-unit-integer code 1) +cache-code-digits+)
          (cache-unit-integer code 2)))))

(defun cache-index-to-code (index)
  (declare (type integer index))
  (multiple-value-bind (hi lo) (floor index +cache-code-digits+)
    (if (zerop hi)
        (format nil "^~A" (code-char (+ lo +base-char-index+)))
        (format nil "^~A~A"
                (code-char (+ hi +base-char-index+))
                (code-char (+ lo +base-char-index+))))))

(defun incf-cache-index ()
  (incf *cache-index*)
  (when (= +max-cache-entries+ *cache-index*)
    (setf *cache-index* 0)
    (setf *cache-map* (empty-map))))

;;;; Interface

(defmacro with-cache (() &body body)
  `(let ((*cache-index* 0)
         (*cache-map* (empty-map))
         (*cache-vector* (make-array +max-cache-entries+)))
     ,@body))

(defun lookup-cache (code)
  (elt *cache-vector* (cache-code-to-index code)))

(defun string-cacheable-p (string)
  (and (char= +esc+ (elt string 0))
       (member (elt string 1) +cached-string-chars+)))

(defun cacheable-p (string key-p)
  (and (> (length string) +min-size-cacheable+)
       (or key-p
           (string-cacheable-p string))))

(defun maybe-cache (string key-p)
  (check-type string string)
  (if (cacheable-p string key-p)
      (cache string)
      string))

;; write caching: name?
(defun cache (value)
  (check-type *cache-index* integer)
  (if-let ((cache-code (lookup *cache-map* value)))
    cache-code
    (progn
      (includef *cache-map* value (cache-index-to-code *cache-index*))
      (incf-cache-index)
      value)))

;; read caching
(defun add-to-cache (string)
  (check-type *cache-index* integer)
  (setf (elt *cache-vector* *cache-index*) string)
  (incf *cache-index*)
  (when (= +max-cache-entries+ *cache-index*)
    (setf *cache-index* 0))
  string)

(defun cache-code-p (string)
  (and (not (emptyp string))
       (char= #\^ (elt string 0))
       (char/= #\Space (elt string 1))))







;;;; tags

;;;; Tags are either CL characters for transit scalar types or CL
;;;; symbols for transit composite types.  Under aesthetic formatting
;;;; they must become the ASCII chars or strings of the transit types
;;;; the represent.

;;;; Impl

(define-constant +ground-character-tags+ '(#\_ #\s #\? #\i #\d #\b) :test #'equal)
(define-constant +ground-composite-tags+ '(|array| |map|) :test #'equal)
(define-constant +map-as-array-tag+ "^ " :test #'string=)

(defgeneric tag (value)
  (:documentation "Return the transit tag for VALUE."))

(defmacro def-simple-tag (method-specializer tag)
  `(defmethod tag ((value ,method-specializer))
     ,tag))

(defclass map-as-array-tag () ())

(defun make-map-as-array-tag ()
  (make-instance 'map-as-array-tag))

(defun map-as-array-tag-p (string)
  (string= +map-as-array-tag+ string))

(defclass composite-tag ()
  ((name :initarg :name)))

(defun make-composite-tag (name)
  (make-instance 'composite-tag :name name))

(defmethod tag ((tag composite-tag))
  (ensure-symbol (slot-value tag 'name) :transit-cl))

(defun quoted-tag-p (tag)
  (and (characterp tag)
       (char= #\' tag)))

(defun ground-tag-p (tag)
  (etypecase tag
    (character (member tag +ground-character-tags+))
    (symbol (member tag +ground-composite-tags+))))

(defun scalar-type-p (value)
  (characterp (tag value)))

;;;; Transit tag definitions

;;; Scalar ground

(def-simple-tag (eql nil) #\_)
(def-simple-tag string #\s)
(def-simple-tag (eql t) #\?)
(def-simple-tag (eql 'true) #\?)
(def-simple-tag (eql 'false) #\?)

(defmethod tag ((integer integer))
  (if (<= (signed-integer-length integer) 64)
      #\i
      #\n)) ; #\n is arbitrary precision integer

(def-simple-tag float #\d)
(def-simple-tag octet-vector #\b)

;;; Scalar extension

(defmethod tag ((value symbol))
  (if (keywordp value)
      #\:
      #\$))

(def-simple-tag arbitrary-decimal #\f)

(defmethod tag ((stamp timestamp))
  (declare (ignore stamp))
  (if *verbose-p*
      #\t
      #\m))

(def-simple-tag uuid:uuid #\u)
(def-simple-tag puri:uri #\r)
(def-simple-tag character #\c)
(def-simple-tag quoted-value #\')
(def-simple-tag (eql :NAN) #\z)
(def-simple-tag (eql :INF) #\z)
(def-simple-tag (eql :-INF) #\z)

;;; Composite ground

(def-simple-tag vector '|array|)
(def-simple-tag fset:seq '|array|)

(defmethod tag ((map fset:map))
  (if (every #'scalar-type-p (domain map))
      '|map|
      '|cmap|))

;;; Composite extension

(def-simple-tag fset:set '|set|)
(def-simple-tag cons '|list|)








;;;; reader

(defgeneric reader (tag value)
  (:documentation "Map VALUE from its JSON/transit parsed type to the
appropriate Common Lisp object indicated by TAG."))

(defun string-escaped-p (string)
  (and (not (emptyp string))
       (char= +esc+ (elt string 0))))

(defun parse-string (string)
  ;; This first test diverges from their read flow chart...
  (cond ((map-as-array-tag-p string)
         (make-map-as-array-tag))
        ((string-escaped-p string)
         (let ((char (elt string 1)))
           (cond ((char= +tag-char+ char)
                  (make-composite-tag (subseq string 2)))
                 ((member char +escape-chars+)
                  (subseq string 1))
                 (t (reader (elt string 1) (subseq string 2))))))
        (t string)))

(defgeneric composite-tagged-p (object)
  (:method ((map fset:map))
    (and (= 1 (size map))
         (typep (arb map) 'composite-tag)))
  (:method ((vector vector))
    (and (plusp (length vector))
         (typep (aref vector 0) 'composite-tag))))

(defun map-as-array-p (vector)
  (and (not (emptyp vector))
       (typep (aref vector 0) 'map-as-array-tag)))

;; transit key/values => Fset map
(defun map-from-key-value-vector (vector)
  (do ((map (empty-map))
       (i (1- (length vector)) (- i 2)))
      ((minusp i) map)
    (includef map (aref vector (1- i)) (aref vector i))))

;;;; Interface for yason parser

(defun read-string (string &key key-p)
  (parse-string (cond ((cacheable-p string key-p)
                       (add-to-cache string))
                      ((cache-code-p string)
                       (lookup-cache string))
                      (t string))))

(defun parse-map (map)
  (if (composite-tagged-p map)
      (multiple-value-bind (tag-name value) (arb map)
        (reader (tag tag-name) value))
      map))

(defun parse-array (vector)
  (if (map-as-array-p vector)
      (reader (ensure-symbol +map-as-array-tag+ :transit-cl) (vector-rest vector))
      (if (composite-tagged-p vector)
          (reader (tag (aref vector 0)) (aref vector 1))
          vector)))

;;;; Transit readers

;;; Scalar, ground

(defmethod reader ((tag (eql #\_)) value)  ;; Test this!!
  (declare (ignore value))
  nil)

(defmethod reader ((tag (eql #\?)) value)
  (if (char= #\t (elt value 0)) t nil))

(defmethod reader ((tag (eql #\i)) value)
  (parse-integer value))

(defmethod reader ((tag (eql #\d)) value)
  ;; see also *read-default-float-format*
  (read-from-string value))

(defmethod reader ((tag (eql #\b)) value)
  (make-octet-vector (cl-base64:base64-string-to-usb8-array value)))

;;; Scalar, extension

(defmethod reader ((tag (eql #\n)) value)
  (parse-integer value))

(defmethod reader ((tag (eql #\:)) value)
  (make-keyword value))

(defmethod reader ((tag (eql #\$)) value)
  (ensure-symbol value))

(defmethod reader ((tag (eql #\f)) value)
  (make-arbitrary-decimal (wu-decimal:parse-decimal value)))

(defmethod reader ((tag (eql #\m)) value)
  (multiple-value-bind (sec msec) (floor (parse-integer value) 1000)
    (unix-to-timestamp sec :nsec (* (expt 10 6) msec))))

(defmethod reader ((tag (eql #\t)) value)
  (parse-rfc3339-timestring value))

(defmethod reader ((tag (eql #\r)) value)
  (puri:parse-uri value))

(defmethod reader ((tag (eql #\u)) value)
  (uuid:make-uuid-from-string value))

(defmethod reader ((tag (eql #\c)) value)
  (elt value 0))

(defmethod reader ((tag (eql #\')) value)
  value)

(defmethod reader ((tag (eql '|'|)) value)
  value)

(defmethod reader ((tag (eql #\z)) value)
  (make-keyword (string-upcase value)))

;;; Scalar extension type: TODO

;;; Composite, ground

;; array is inherent to parse-array

;; map is inherent to parse-map but we also need to read the
;; array-as-map format
(defmethod reader ((tag (eql '|^ |)) (vector vector))
  (map-from-key-value-vector vector))

;;; Composite, extension

;; set
(defmethod reader ((tag (eql '|set|)) (vector vector))
  (convert 'set vector))

;; list
(defmethod reader ((tag (eql '|list|)) (vector vector))
  (coerce vector 'list))

;; cmap
(defmethod reader ((tag (eql '|cmap|)) (vector vector))
  (map-from-key-value-vector vector))

;; link TODO





;;;; rep
;;;; We need only define rep for extension types (scalar and composite)

(defgeneric rep (object))

;;; Scalar

;; Now we need only define string-rep when rep is not a string
(defgeneric string-rep (object)
  (:method (object)
    (rep object)))

(defmethod rep ((symbol symbol))
  (format nil "~A" symbol))

(defmethod rep ((object arbitrary-decimal))
  (let ((*print-pprint-dispatch* *transit-pprint-dispatch*)
        (*print-pretty* t))
    (with-slots (value) object
      (format nil "~A" value))))

(defmethod rep ((integer integer))
  (format nil "~D" integer))

(defmethod rep ((stamp timestamp))
  (timestamp-to-epoch-msec stamp))

(defmethod string-rep ((stamp timestamp))
  (if *verbose-p*
      (rfc3339-string stamp)
      (format nil "~D" (rep stamp))))

(defmethod rep ((uuid uuid:uuid))
  uuid)

(defmethod string-rep ((uuid uuid:uuid))
  (string-downcase (format nil "~A" uuid)))

(defmethod rep ((uri puri:uri))
  (format nil "~A" uri))

(defmethod rep ((char character))
  (format nil "~A" char))

(defmethod rep ((object quoted-value))
  (slot-value object 'value))

(defmethod rep ((special-number (eql :nan)))
  "NaN")

(defmethod rep ((special-number (eql :inf)))
  "INF")

(defmethod rep ((special-number (eql :-inf)))
  "-INF")

;;; Composite

(defmethod rep ((set fset:set))
  (convert 'vector set))

(defmethod rep ((list cons))
  (coerce list 'vector))

(defmethod rep ((cmap fset:map))
  (let ((vec (make-array (* 2 (size cmap)) :fill-pointer 0)))
    (do-map (key value cmap vec)
      (vector-push key vec)
      (vector-push value vec))))

;; TODO link





;;;; writer

;;;; String escaping

(defun escape-string-p (string)
  (and (not (emptyp string))
       (member (elt string 0) +escape-chars+)))

(defun escape-string (string)
  (if (escape-string-p string)
      (format nil "~~~A" string)
      string))

;;;; Fset map => transit key/values

(defun map-as-key-value-vector (map first)
  (let ((len (1+ (* 2 (size map)))))
    (let ((vec (make-array len :fill-pointer 0)))
      (vector-push first vec)
      (do-map (key value map vec)
        (vector-push (marshall key :key-p t) vec)
        (vector-push (marshall value) vec)))))

;;;; Emitting and marshalling

(defun emit-string (tag string key-p)
  (let ((string (format nil "~~~A~A" tag string)))
    (if *verbose-p*
        string
        (maybe-cache string key-p))))

(defun emit-tagged (tag rep)
  (let ((meta (format nil "~~#~A" tag)))
    (if *verbose-p*
        (map (meta (marshall rep)))
        (seq (maybe-cache meta nil) (marshall rep)))))

(defun marshall (object &key key-p)
  (let ((tag (tag object)))
    (if (quoted-tag-p tag)
        (emit-tagged tag (rep object))
        (if (ground-tag-p tag)
            (emit-ground tag object key-p)
            (etypecase tag
              (character (emit-string tag (string-rep object) key-p))
              (symbol (emit-tagged tag (rep object))))))))

;;;; emit-ground, scalar methods

(defgeneric emit-ground (tag object key-p))

(defmethod emit-ground ((tag (eql #\_)) value key-p)
  (declare (ignore value))
  (if (not key-p)
      nil
      (emit-string tag "" key-p)))

(defmethod emit-ground ((tag (eql #\s)) (string string) key-p)
  (let ((string (escape-string string)))
    (if *verbose-p*
        string
        (maybe-cache string key-p))))

(defmethod emit-ground ((tag (eql #\?)) value key-p)
  (if (not key-p)
      value
      (emit-string tag (if (eq 'true value) "t" "f") key-p)))

(defmethod emit-ground ((tag (eql #\i)) (integer integer) key-p)
  (if (and (not key-p)
           (< (signed-integer-length integer) 53))
      integer
      (emit-string tag (format nil "~D" integer) key-p)))

(defmethod emit-ground ((tag (eql #\d)) (float float) key-p)
  (if (not key-p)
      float
      (emit-string tag (format nil "~F" float) key-p)))

(defmethod emit-ground ((tag (eql #\b)) (object octet-vector) key-p)
  (with-slots (value) object
    (let ((bytes (cl-base64:usb8-array-to-base64-string value)))
      (emit-string tag bytes key-p))))

;;;; emit-ground, composite methods

(defmethod emit-ground ((tag (eql '|array|)) (vector vector) key-p)
  (declare (ignore key-p))
  (cl:map 'vector #'marshall vector))

(defmethod emit-ground ((tag (eql '|array|)) (seq fset:seq) key-p)
  (declare (ignore key-p))
  (image #'marshall seq))

(defmethod emit-ground ((tag (eql '|map|)) (map fset:map) key-p)
  (declare (ignore key-p))
  (if *verbose-p*
      (image (lambda (key value)
               (values (marshall key :key-p t)
                       (marshall value)))
             map)
      (map-as-key-value-vector map +map-as-array-tag+)))





;;;; yason

;;; Here we extend yason with some functions similar to the originals
;;; but customized for our purposes.  Regular yason clients in same
;;; image should be unaffected.

(in-package :yason)

(defun transit-parse-array (input &key key-p)
  (let ((return-value (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
    (read-char input)
    (let (map-as-array-p
          (i 0))
      (loop
        (when (eql (peek-char-skipping-whitespace input)
                   #\])
          (return))
        (let ((value (cond ((zerop i)
                            (let ((value (parse-transit% input :key-p key-p)))
                              (when (typep value 'transit-cl::map-as-array-tag)
                                (setf map-as-array-p t))
                              value))
                           ((and map-as-array-p (oddp i))
                            (parse-transit% input :key-p t))
                           (t
                            (parse-transit% input :key-p key-p)))))
          (vector-push-extend value return-value))
        (incf i)
        (ecase (peek-char-skipping-whitespace input)
          (#\, (read-char input))
          (#\] nil))))
    (read-char input)
    return-value))

;; New
(defun transit-parse-object (input)
  (let ((return-value (fset:empty-map)))
    (read-char input)
    (loop
      (when (eql (peek-char-skipping-whitespace input)
                 #\})
        (return))
      (skip-whitespace input)
      (setf return-value
            (fset:with return-value
                       (let ((key-string (parse-string input)))
                         (prog1
                             (transit-cl::read-string key-string :key-p t)
                           (skip-whitespace input)
                           (unless (eql #\: (read-char input))
                             (error 'expected-colon :key-string key-string))
                           (skip-whitespace input)))
                       (parse-transit% input)))
      (ecase (peek-char-skipping-whitespace input)
        (#\, (read-char input))
        (#\} nil)))
    (read-char input)
    return-value))

;; Replace
(remove-method #'yason:encode (find-method #'yason:encode '() (mapcar #'find-class '(float))))
(defmethod encode ((object float) &optional (stream *standard-output*))
  (princ object stream))

;; New
(defmethod encode ((object fset:seq) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\[ #\])
    (fset:do-seq (value object :value object)
      (with-element-output ()
        (encode value stream)))))

;; New
(defmethod encode ((object fset:map) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\{ #\})
    (fset:image (lambda (key value)
                  (with-element-output ()
                    (encode-key/value key value stream)))
                object)
    object))

;; New, for use below
(defgeneric parse-transit% (input &key key-p)
  (:method ((input stream) &key key-p)
    (ecase (peek-char-skipping-whitespace input)
      (#\"
       (transit-cl::read-string (parse-string input) :key-p key-p))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (parse-number input))
      (#\{
       (transit-cl::parse-map (transit-parse-object input)))
      (#\[
       (transit-cl::parse-array (transit-parse-array input :key-p key-p)))
      ((#\t #\f #\n)
       (parse-constant input))))
  (:method ((input pathname) &key key-p)
    (with-open-file (stream input)
      (parse-transit% stream :key-p key-p)))
  (:method ((input string) &key key-p)
    (parse-transit% (make-string-input-stream input) :key-p key-p)))

;; New entry point for parsing transit flavoured JSON
(defun parse-transit (input)
  (let ((*parse-object-as* :fset-map)
        (*parse-json-arrays-as-vectors* nil)
        (*parse-json-booleans-as-symbols* nil)
        (*parse-json-null-as-keyword* nil))
    ;; parse is called recursively within yason, so...
    (let ((original-parse (symbol-function 'parse)))
      (setf (symbol-function 'parse) #'parse-transit%)
      (unwind-protect (parse-transit% input)
        (setf (symbol-function 'parse) original-parse)))))




;;;; extension

(in-package :transit-cl)

;;; Fset Seq

;; (def-simple-tag fset:seq "seq")

;; (defmethod writer ((seq fset:seq))
;;   (write-tagged (tag seq) (lambda () (image #'writer seq))))

;; (defmethod read-tagge ((tag (eql '|seq|)) list)
;;   (gmap :seq #'reader (:list list)))

;; Ratios (that aren't wu-decimals)
(def-simple-tag ratio '|ratio|)

(defmethod rep ((ratio ratio))
  (vec (numerator ratio) (denominator ratio)))

(defmethod reader ((tag (eql '|ratio|)) (vector vector))
  (/ (aref vector 0) (aref vector 1)))

;;; Points
(defstruct point x y)

(def-simple-tag point '|point|)

(defmethod rep ((point point))
  (vec (point-x point) (point-y point)))

(defmethod reader ((tag (eql '|point|)) (vector vector))
  (make-point :x (aref vector 0) :y (aref vector 1)))

(defstruct rsize width height)

(def-simple-tag rsize '|rsize|)

(defmethod rep ((rsize rsize))
  (vec (rsize-width rsize) (rsize-height rsize)))

(defstruct rect origin size)

(def-simple-tag rect '|rect|)

(defmethod rep ((rect rect))
  (vec (rect-origin rect) (rect-size rect)))




;;;; interface

(defun transit-read (input)
  "Parse the transit format from INPUT, which must be a string or
stream, into Lisp objects."
  (with-cache ()
    (yason::parse-transit input)))

(defun transit-write (object &optional (stream *standard-output*))
  "Serialize Lisp OBJECT as transit to STREAM."
  (with-cache ()
    (yason:encode (if (scalar-type-p object)
                      (marshall (make-quoted-value object))
                      (marshall object))
                  stream)))

(defun transit-write-string (object)
  "Serialize Lisp OBJECT to a transit string."
  (with-output-to-string (stream)
    (transit-write object stream)))

(defgeneric roundtrip (object)
  (:method (object)
    (transit-read (transit-write-string object)))
  (:method ((string string))
    (transit-write-string (transit-read string))))
