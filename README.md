transit-cl
==========

## What is it?

A Common Lisp implementation of the [transit
format](https://github.com/cognitect/transit-format).

## Status

As of early April 2015 it is more than a proof of concept but less
than a pre-release: the test suite is incomplete and no work has
been done on documentation.

## The name

I recognize that the use of the name `transit-cl` is somewhat
presumptious, it represents an optimisim about making this a community
implementation.  I will happily release it upon request.

## Quickstart

Clone this repo to your `~/quicklisp/local-projects` directory, then:

```common-lisp
(ql:register-local-projects)
(ql:quickload :transit-cl)
```

```
(transit-write-string (vec (map ("bill" 10) ("ben" 11))
                           (set 1 'two :three "four")
                           (list 1 2 3)
                           98765432123456789
                           (exp 1)
                           (make-bytes '(65 66 67))
                           (now)
                           (make-decimal "123456789.123456789")
                           (uuid:make-v4-uuid)
                           (make-point :x 1.1 :y 2.2)))
"[[\"^ \",\"ben\",11,\"bill\",10],[\"~#set\",[1,\"~:THREE\",\"~$TWO\",\"four\"]],[\"~#list\",[1,2,3]],\"~i98765432123456789\",2.7182817,\"~bQUJD\",\"~m1427774432906\",\"~f123456789.123456789\",\"~u9d3d4b49-c2de-4127-8060-abb4f14da0b8\",[\"~#point\",[1.1,2.2]]]"

(princ *)
[["^ ","ben",11,"bill",10],["~#set",[1,"~:THREE","~$TWO","four"]],["~#list",[1,2,3]],"~i98765432123456789",2.7182817,"~bQUJD","~m1427777100568","~f123456789.123456789","~u21801e12-6c37-4247-be24-7aaa8622972f",["~#point",[1.1,2.2]]]

(transit-read *)
#(#{| ("ben" 11) ("bill" 10) |} #{ 1 :THREE TWO "four" } (1 2 3)
  98765432123456789 2.7182817 #<OCTET-VECTOR #(65 66 67)>
  @2015-03-31T15:00:32.906000+11:00 #<ARBITRARY-DECIMAL #$123456789.123456789>
  9D3D4B49-C2DE-4127-8060-ABB4F14DA0B8 #S(POINT :X 1.1 :Y 2.2))
```

Note the integral use of [Fset](https://github.com/slburson/fset) maps and sets.  It turns out
Common Lisp natively supports most of the transit type menagerie, with the need for the use of
just a few other quicklisp libraries such as Fset.

## Tests

```
(ql:quickload :transit-cl-test)
(in-package :transit-cl)
(5am:run! :basic)
```
