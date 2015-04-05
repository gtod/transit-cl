transit-cl
==========

## What is it?

A Common Lisp implementation of the [transit
format](https://github.com/cognitect/transit-format).

## Status

As of early April 2015 this is a pre-release: there interface may
still change and the test suite is incomplete.  Scalar extensions,
among other things, are not yet implemented.

## The name

I recognize that the use of the name `transit-cl` is somewhat
presumptious, it represents an optimism about making this a community
implementation.  I will happily release it to Cognitect upon request.

## Quickstart

Clone this repo to your `~/quicklisp/local-projects` directory, then:

```common-lisp
(ql:register-local-projects)
(ql:quickload :transit-cl)
```

```common-lisp
(defpackage :example
  (:use :cl :transit-cl)
  (:shadowing-import-from :fset #:map #:set))

(in-package :example)

;; See https://github.com/cognitect/transit-format for a list of
;; transit's Scalar and Composite types, exemplified here.
(transit-write-string (vec ;; Scalar
                           nil
                           "string"
                           'true 'false
                           123
                           123.456
                           (make-bytes '(65 66 67))
                           :key
                           'sym
                           (make-decimal "123.456")
                           (local-time:now)
                           (uuid:make-v4-uuid)
                           (puri:parse-uri "http://foo.bar.com")
                           #\c
                           (make-quoted-value "value")
                           :nan :inf :-inf
                           ;; Composite
                           (vec 1 2 3)
                           (map ("bill" 10) ("ben" 11))
                           (set 1 'two :three "four")
                           (list 1 2 3)
                           (map ((vec "composite map") "value"))
                           ;; Extension
                           (make-point :x 1.1 :y 2.2)))
"[null,\"string\",true,false,123,123.456,\"~bQUJD\",\"~:KEY\",\"~$SYM\",\"~f123.456\",\"~m1427952719132\",\"~u1f316393-06f5-41d9-9349-9475e396db78\",\"~rhttp://foo.bar.com/\",\"~cc\",[\"~#'\",\"value\"],\"~zNaN\",\"~zINF\",\"~z-INF\",[1,2,3],[\"^ \",\"ben\",11,\"bill\",10],[\"~#set\",[1,\"~$TWO\",\"~:THREE\",\"four\"]],[\"~#list\",[1,2,3]],[\"~#cmap\",[[\"composite map\"],\"value\"]],[\"~#point\",[1.1,2.2]]]"

(princ *)
[null,"string",true,false,123,123.456,"~bQUJD","~:KEY","~$SYM","~f123.456","~m1427952526698","~u93543b04-0b99-4dcb-9b9f-36eac5493d9e","~rhttp://foo.bar.com/","~cc",["~#'","value"],"~zNaN","~zINF","~z-INF",[1,2,3],["^ ","ben",11,"bill",10],["~#set",[1,"~$TWO","~:THREE","four"]],["~#list",[1,2,3]],["~#cmap",[["composite map"],"value"]],["~#point",[1.1,2.2]]]

(transit-read *)
#(NIL "string" T NIL 123 123.456 #<TRANSIT-CL::OCTET-VECTOR #(65 66 67)> :KEY
  SYM #<TRANSIT-CL::ARBITRARY-DECIMAL #$123.456>
  @2015-04-02T16:28:46.698000+11:00 93543B04-0B99-4DCB-9B9F-36EAC5493D9E
  #<PURI:URI http://foo.bar.com/> #\c "value" :NAN :INF :-INF #(1 2 3)
  #{| ("ben" 11) ("bill" 10) |} #{ 1 TWO :THREE "four" } (1 2 3)
  #{| (#("composite map") "value") |} #S(TRANSIT-CL::POINT :X 1.1 :Y 2.2))
```

Note the integral use of [Fset](https://github.com/slburson/fset) maps and sets.  It turns out
Common Lisp natively supports most of the transit type menagerie, with the need for the use of
just a few other quicklisp libraries such as Fset.

## Tests

Clone `https://github.com/cognitect/transit-format` somewhere locally,
so the exemplar tests can be run, like so:

```common-lisp
(ql:quickload :transit-cl-test)
(setf transit-cl-exemplars:*exemplars-dir* "/somewhere/transit-format/examples/0.8/simple/")
(asdf:test-system :transit-cl)
```

## Documentation

### Interface

```common-lisp
(transit-write OBJECT &optional (STREAM *standard-output*))
```
Writes the Lisp object OBJECT to STREAM in transit format.

```common-lisp
(transit-write-string OBJECT)
```
Writes the Lisp object to a string of transit.

```common-lisp
(transit-read STRING)
```
Parses the transit format STRING to Lisp objects.

```common-lisp
(verbosely FORM)
```
A macro used to wrap a single invocation of `transit-write` or
`transit-write-string` to ensure output in the verbose transit format.

```common-lisp
(vec &rest LIST)
```
Make a vector with LIST as the initial contents.

```common-lisp
(make-quoted-value VALUE)
```
Ensure that Lisp object VALUE is quoted in the transit output.  There
is little call for doing this manually, `transit-write` ensures it is
done for top level scalar objects.

```common-lisp
(make-bytes LIST)
```
Make an octet vector of bytes, boxed in a transit ready type, from the
LIST of integers.

```common-lisp
(make-decimal STRING)
```
Wrap a [wu-decimal](https://wukix.com/lisp-decimals) in a transit
ready box type from STRING.

#### Behaviour of nil

In CL, `nil` serves as both the empty list and boolean false.  The
transit format explicitly supports lists but rather than have `nil`
become an empty transit list it maps to JSON `null`.  The symbols
`yason:false` and `yason:true` map to their JSON equivalents (they are
exported from the transit-cl package):

```common-lisp
(transit-write-string (vec nil (list) (list 1) 'false 'true t))
"[null,null,[\"~#list\",[1]],false,true,true]"

(transit-read *)
#(NIL NIL (1) NIL T T)
```

Note that both transit/JSON `null` and `false` are parsed as CL `nil`.
