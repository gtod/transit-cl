(in-package :transit-cl)

(def-suite :transit-cl)
(def-suite :basic :in :transit-cl)
(in-suite :basic)

(defun dual (left right)
  (is (string= left (transit-write-string right)))
  (is (equal? right (transit-read left))))

(defmacro test-roundtrip (name &body strings)
  `(test ,name
     ,@(mapcar (lambda (string)
                 `(is (string= ,string (roundtrip ,string))))
               strings)))

(test writer
  (dual "[\"^ \",\"foo\",\"bar\"]" (map ("foo" "bar")))
  (dual "[[\"^ \",\"foobar\",\"foobar\"],[\"^ \",\"^0\",\"foobar\"]]"
         (vec (map ("foobar" "foobar")) (map ("foobar" "foobar"))))
  (verbosely
   (progn
     (dual "{\"foo\":\"bar\"}" (map ("foo" "bar")))
     (dual "[{\"foobar\":\"foobar\"},{\"foobar\":\"foobar\"}]"
           (vec (map ("foobar" "foobar")) (map ("foobar" "foobar"))))
     (let ((stamp "2015-01-02T14:14:14.987Z"))
       (dual (format nil "{\"~~#'\":\"~~t~A\"}" stamp)
             (parse-rfc3339-timestring stamp))))))

(test reader
  (verbosely (dual "{\"foo\":\"bar\"}" (map ("foo" "bar")))))

(test transit-types
  (dual "[\"foo\"]" #("foo"))
  (dual "[1]" #(1))
  (dual "[\"~i9007199254740993\"]" #(9007199254740993))
  (dual "[\"~i-9007199254740993\"]" #(-9007199254740993))
  (is (string= "[1.5]" (transit-write-string #(1.5))))
  (is (string= "[true]" (transit-write-string #(t))))
  (is (string= "[true]" (transit-write-string #(true))))
  (is (string= "[false]" (transit-write-string #(false))))
  (dual "[\"~:FOO\"]" #(:foo))
  (dual "[\"~:foo\"]" #(:|foo|))
  (dual "[\"~$FOO\"]" #(foo))
  (dual "[\"~$foo\"]" #(|foo|))
  (dual "[\"~m482196050052\"]" (vec (epoch-msec-to-timestamp 482196050052)))
  (dual "[\"~:FOO\",\"~$BAR\"]" #(:foo bar))
  (dual "[\"~$FOO\",\"~:BAR\"]" #(foo :bar))
  (dual "[\"~rhttp://foo.com/\"]" (vec (puri:parse-uri "http://foo.com/")))
  (dual "[\"~#list\",[1,2,3]]" (list 1 2 3))
  (dual "[[\"~#list\",[1,2,3]]]" (vec (list 1 2 3)))
  (dual "[\"~#\'\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"]"
        (uuid:make-uuid-from-string "531a379e-31bb-4ce1-8690-158dceb64be6"))
  (verbosely (dual "{\"~#\'\":\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"}"
                   (uuid:make-uuid-from-string "531a379e-31bb-4ce1-8690-158dceb64be6")))
  (dual "[\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"]"
        (vec (uuid:make-uuid-from-string "531a379e-31bb-4ce1-8690-158dceb64be6")))
  (dual "[\"~bQUJD\"]" (vec (make-bytes '(65 66 67))))
  (dual "[\"~f111.123456789012\"]" (vec (make-decimal "111.123456789012")))
  (dual "[\"~cX\"]" #(#\X))
  (dual "[\"~zNaN\"]" #(:nan))
  (dual "[\"~zINF\"]" #(:inf))
  (dual "[\"~z-INF\"]" #(:-inf)))

(test list
  (dual "[\"~#list\",[\"foo\",\"bar\",\"baz\"]]" (list "foo" "bar" "baz")))

(test round-trip
  (is (equal? (transit-read (transit-write-string '("foo" "bar" "baz")))
              '("foo" "bar" "baz"))))

(test write-edge-cases
  (dual "[[1,2]]" (vec (vec 1 2)))
  (dual "[[1,2],[3,4]]" (vec (vec 1 2) (vec 3 4)))
  (dual "[[[1,2]]]" (vec (vec (vec 1 2))))
  (dual "[[\"^ \",\"foo\",[1,2]]]" (vec (map ("foo" (vec 1 2)))))
  ;; This is one answer as to what empty maps should look like...
  (dual "[[\"^ \",\"foo\",[1,2,[\"^ \"]]]]" (vec (map ("foo" (vec 1 2 (empty-map)))))))

(test extension
  (is (string= "[\"~#point\",[1.5,2.5]]" (transit-write-string (make-point :x 1.5 :y 2.5))))
  (is (string= "[\"~#cmap\",[[\"~#point\",[1.5,2.5]],1]]"
               (transit-write-string (map ((make-point :x 1.5 :y 2.5) 1))))))

(test write-cmap
  (dual "[\"~#cmap\",[[1,2],\"foo\"]]" (map ((vec 1 2) "foo")))
  (verbosely (dual "{\"~#cmap\":[[1,2],\"foo\"]}" (map ((vec 1 2) "foo")))))

;; Links, todo
;; exports.testLink = function(test) {
;;     var w = transit.writer(),
;;         r = transit.reader(),
;;         l = r.read("{\"~#link\":{\"href\":\"~rhttp://foo.com\",\"rel\":\"a-rel\",\"name\":\"a-name\",\"render\":\"image\",\"prompt\":\"a-prompt\"}}");

;;     test.ok(transit.isURI(l.rep.get("href")));
;;     test.equal(l.rep.get("rel"), "a-rel");
;;     test.equal(l.rep.get("name"), "a-name");
;;     test.equal(l.rep.get("render"), "image");
;;     test.equal(l.rep.get("prompt"), "a-prompt");

;;     test.done();
;; };

(test verify-array-hash
  (dual "[\"^ \",\"~:FOO\",\"bar\"]" (map (:foo "bar")))
  (is (equal? (transit-read "[\"^ \",\"~:FOO\",\"^0\"]") (map (:foo :foo)))))

(test read-array-tagged-value
  (let ((set (transit-read "[\"~#set\",[1,2,3]]")))
    (is (fset:set? set))
    (is (= 3 (size set)))
    (is (fset:contains? set 1))
    (is (fset:contains? set 2))
    (is (fset:contains? set 3))))

(test read-error
  (is (eq :err (handler-case (transit-read "[\"~q1\"]")
                 (error ()
                   :err)))))

(test tag-edge-cases
  (is (equal? (transit-read "{\"~~:set\":[1,2,3]}")
              (map ("~:set" (vec 1 2 3)))))
  (is (equal? (transit-read "[\"^ \",\"~~:set\",[1,2,3]]")
              (map ("~:set" (vec 1 2 3)))))
  (is (equal? (transit-read"[{\"~~:set\":[1,2,3]},{\"^0\":[1,2,3]}]")
              (vec (map ("~:set" (vec 1 2 3)))
                   (map ("~:set" (vec 1 2 3))))))
  (is (equal? (transit-read "[[\"^ \",\"~~:set\",[1,2,3]],[\"^ \",\"^0\",[1,2,3]]]")
              (vec (map ("~:set" (vec 1 2 3)))
                   (map ("~:set" (vec 1 2 3)))))))

(test verify-roundtrip-cache-keys
  (is (string= (roundtrip "[\"~:foo\",\"~:bar\",[\"^ \",\"^1\",[1,2]]]")
               "[\"~:foo\",\"~:bar\",[\"^ \",\"^1\",[1,2]]]")))

(test test-quoted
  (dual "[\"~#\'\",null]" nil)
  (dual "[\"~#\'\",1]" 1)
  (dual "[\"~#\'\",2.5]" 2.5)
  (dual "[\"~#\'\",\"foo\"]" "foo")
  (dual "[\"~#\'\",true]" t)
  ;; (dual "[\"~#\'\",false]" 'false) ; nil is null...
  (dual "[\"~#\'\",\"~m0\"]" (epoch-msec-to-timestamp 0))
  (dual "[\"~#\'\",\"~i4953778853208128465\"]" 4953778853208128465)
  (dual "[\"~#\'\",\"~n8987676543234565432178765987645654323456554331234566789\"]"
        8987676543234565432178765987645654323456554331234566789))

(test-roundtrip verify-json-corner-cases
  "[\"~#point\",[1,2]]"
  ;; "[\"^ \",\"~/t\",null]"   ??
  ;; "[\"^ \",\"~/f\",null]"
  ;; "{\"~#'\":\"~f-1.1E-1\"}"  ??
  ;; "{\"~#'\":\"~f-1.10E-1\"}"
  "[\"~#list\",[[\"~#ratio\",[\"~i4953778853208128465\",\"~i636801457410081246\"]],[\"^1\",[\"~i-8516423834113052903\",\"~i5889347882583416451\"]]]]"
  )

(test-roundtrip verify-roundtrip-cmap
  "[\"~#cmap\",[[1,1],\"one\"]]"
  "[\"~#cmap\",[[\"~:foo\",1],[[\"^ \",\"~:bar\",2],[\"^ \",\"^2\",3]]]]")

(test-roundtrip verify-roundtrip-map-cache-keys
  "[[\"^ \",\"aaaa\",1,\"bbbb\",2],[\"^ \",\"^0\",3,\"^1\",4],[\"^ \",\"^0\",5,\"^1\",6]]")

(test-roundtrip verify-roundtrip-empty-string
  "[\"\",\"a\",\"ab\",\"abc\",\"abcd\",\"abcde\",\"abcdef\"]")

(test-roundtrip verify-roundtrip-big-integer
  "[\"~#'\",\"~n8987676543234565432178765987645654323456554331234566789\"]")

(test roundtrip-long-key
  (is (equal? (transit-read "\{\"~i1\":\"foo\"}")
              (map (1 "foo")))))

;; (test disable-write-caching
;;     ;; var writer = transit.writer("json", {cache: false});
;;     ;; test.equal(writer.write([transit.keyword("foo"), transit.keyword("foo")]), "[\"~:foo\",\"~:foo\"]");
;;   )

(test roundtrip-verbose-dates
  (let ((json "[\"~t1776-07-04T12:00:00.000Z\",\"~t1970-01-01T00:00:00.000Z\",\"~t2000-01-01T12:00:00.000Z\",\"~t2014-04-07T22:17:17.000Z\"]"))
    (is (string= json (verbosely (transit-write-string (transit-read json)))))))

(test roundtrip-big-integer
  ;; It doesn't make sense to roundtrip "[\"~n1\"]" (as in the JS test
  ;; suite) because there is no separate "big integer" type in CL, and
  ;; 1 is written out as just 1...
  (is (string= "[1]" (transit-write-string (transit-read "[\"~n1\"]")))))

(test roundtrip-uuid-corner-case
  (is (string= "[\"~#'\",\"~u2f9e540c-0591-eff5-4e77-267b2cb3951f\"]"
               (transit-write-string (transit-read "{\"~#'\":\"~u2f9e540c-0591-eff5-4e77-267b2cb3951f\"}")))))

(test-roundtrip map-corner-case
  "[\"^ \"]")

(test-roundtrip map-key-ratio-case
  "[\"~#cmap\",[[\"~#ratio\",[1,2]],[\"^1\",[2,5]]]]"
  "[\"~#cmap\",[[\"~#ratio\",[10,13]],\"~:foobar\",[\"^1\",[10,11]],\"^2\"]]")

(test-roundtrip roundtrip-escaped-string
  "[\"~#\'\",\"~\`~hello\"]")

(test-roundtrip rolling-cache-edge-case
  "[\"~:key0000\",\"~:key0001\",\"~:key0002\",\"~:key0003\",\"~:key0004\",\"~:key0005\",\"~:key0006\",\"~:key0007\",\"~:key0008\",\"~:key0009\",\"~:key0010\",\"~:key0011\",\"~:key0012\",\"~:key0013\",\"~:key0014\",\"~:key0015\",\"~:key0016\",\"~:key0017\",\"~:key0018\",\"~:key0019\",\"~:key0020\",\"~:key0021\",\"~:key0022\",\"~:key0023\",\"~:key0024\",\"~:key0025\",\"~:key0026\",\"~:key0027\",\"~:key0028\",\"~:key0029\",\"~:key0030\",\"~:key0031\",\"~:key0032\",\"~:key0033\",\"~:key0034\",\"~:key0035\",\"~:key0036\",\"~:key0037\",\"~:key0038\",\"~:key0039\",\"~:key0040\",\"~:key0041\",\"~:key0042\",\"~:key0043\",\"~:key0044\",\"~:key0045\",\"~:key0046\",\"~:key0047\",\"~:key0048\",\"~:key0049\",\"~:key0050\",\"~:key0051\",\"~:key0052\",\"~:key0053\",\"~:key0054\",\"~:key0055\",\"~:key0056\",\"~:key0057\",\"~:key0058\",\"~:key0059\",\"~:key0060\",\"~:key0061\",\"~:key0062\",\"~:key0063\",\"~:key0064\",\"~:key0065\",\"~:key0066\",\"~:key0067\",\"~:key0068\",\"~:key0069\",\"~:key0070\",\"~:key0071\",\"~:key0072\",\"~:key0073\",\"~:key0074\",\"~:key0075\",\"~:key0076\",\"~:key0077\",\"~:key0078\",\"~:key0079\",\"~:key0080\",\"~:key0081\",\"~:key0082\",\"~:key0083\",\"~:key0084\",\"~:key0085\",\"~:key0086\",\"~:key0087\",\"~:key0088\",\"~:key0089\",\"~:key0090\",\"~:key0091\",\"~:key0092\",\"~:key0093\",\"~:key0094\",\"^0\",\"^1\",\"^2\",\"^3\",\"^4\",\"^5\",\"^6\",\"^7\",\"^8\",\"^9\",\"^:\",\"^;\",\"^<\",\"^=\",\"^>\",\"^?\",\"^@\",\"^A\",\"^B\",\"^C\",\"^D\",\"^E\",\"^F\",\"^G\",\"^H\",\"^I\",\"^J\",\"^K\",\"^L\",\"^M\",\"^N\",\"^O\",\"^P\",\"^Q\",\"^R\",\"^S\",\"^T\",\"^U\",\"^V\",\"^W\",\"^X\",\"^Y\",\"^Z\",\"^[\",\"^10\",\"^11\",\"^12\",\"^13\",\"^14\",\"^15\",\"^16\",\"^17\",\"^18\",\"^19\",\"^1:\",\"^1;\",\"^1<\",\"^1=\",\"^1>\",\"^1?\",\"^1@\",\"^1A\",\"^1B\",\"^1C\",\"^1D\",\"^1E\",\"^1F\",\"^1G\",\"^1H\",\"^1I\",\"^1J\",\"^1K\",\"^1L\",\"^1M\",\"^1N\",\"^1O\",\"^1P\",\"^1Q\",\"^1R\",\"^1S\",\"^1T\",\"^1U\",\"^1V\",\"^1W\",\"^1X\",\"^1Y\",\"^1Z\",\"^1[\",\"^20\",\"^21\",\"^22\",\"^23\",\"^24\",\"^25\",\"^26\"]")

(test map-marker-edge-case
  (is (equal? (vec "^ ") (roundtrip (vec "^ "))))
  (is (equal? (vec "^ " 1 "^ ") (roundtrip (vec "^ " 1 "^ "))))
  (is (equal? (map ("^ " "^ ")) (verbosely (roundtrip (map ("^ " "^ ")))))))

(test composite-tag-edge-case
  (is (fset:set? (transit-read (transit-write-string (set 1 2 3)))))
  (is (vectorp (transit-read (transit-write-string (vec "~#set" (vec 1 2 3)))))))

(test-roundtrip roundtrip-composite-tag-edge-case
  "[\"~~#set\",[1,2,3]]"
  "[\"~#set\",[1,2,3]]")

#|||


// =============================================================================
// Custom tags
// =============================================================================

exports.testTag = function(test) {
    var Point2d = function(x, y) {
        this.x = x;
        this.y = y;
    };

    Point2d.prototype.transitTag = "point";

    var Point3d = function(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    };

    Point3d.prototype.transitTag = "point";

    var w = transit.writer("json", {
        "handlers": transit.map([
            "point", transit.makeWriteHandler({
                tag: function(v) {
                    if(v instanceof Point2d) {
                        return "point/2d";
                    } else if(v instanceof Point3d) {
                        return "point/3d";
                    }
                },
                rep: function(v) {
                    if(v instanceof Point2d) {
                        return [v.x, v.y];
                    } else if(v instanceof Point3d){
                        return [v.x, v.y, v.z];
                    }
                },
                stringRep: function(v) {
                    return null;
                }
            })
        ])
    });

    test.equal(w.write([new Point2d(1.5,2.5), new Point3d(1.5,2.5,3.5)]),
               '[["~#point/2d",[1.5,2.5]],["~#point/3d",[1.5,2.5,3.5]]]');

    test.done();
};

// =============================================================================
// Default write handler
// =============================================================================

exports.testDefaultWriteHandler = function(test) {
    var Point = function(x, y) {
        this.x = x;
        this.y = y;
    };

    Point.prototype.transitRep = function() {
        return {
            tag: "point",
            x: this.x,
            y: this.y
        }
    };

    var DefaultHandler = transit.makeWriteHandler({
        tag: function(v, h) { return "unknown"; },
        rep: function(v, h) { return v.transitRep(); }
    });

    var w = transit.writer("json", {
        "handlers": transit.map([
            "default", DefaultHandler
        ])
    });

    test.equal(w.write(new Point(1.5,2.5)),
               "[\"~#unknown\",[\"^ \",\"tag\",\"point\",\"x\",1.5,\"y\",2.5]]");

    test.done();
};

// =============================================================================
// Special Double Values
// =============================================================================

exports.testReadSpecialDoubleValues = function(test) {
    var r = transit.reader();

    test.ok(isNaN(r.read("[\"~#'\",\"~zNaN\"]")));
    test.equal(r.read("[\"~#'\",\"~zINF\"]"), Infinity);
    test.equal(r.read("[\"~#'\",\"~z-INF\"]"), -Infinity);

    test.done();
};

exports.testWriteSpecialDoubleValues = function(test) {
    var w = transit.writer();

    test.equal(w.write(Infinity), "[\"~#'\",\"~zINF\"]")
    test.equal(w.write(-Infinity), "[\"~#'\",\"~z-INF\"]");
    test.equal(w.write(NaN), "[\"~#'\",\"~zNaN\"]");

    test.done();
};

// =============================================================================
// Map Not Found
// =============================================================================

exports.testMapGetNotFound = function(test) {
    var m = transit.map([
        "foo", 1,
        "bar", 2
    ]);

    test.equal(m.get("baz", "woz"), "woz");

    test.done();
};

// =============================================================================
// Functions as keys
// =============================================================================

exports.testFunctionsAsKeys = function(test) {
    var isEven = function(n) { return n % 2 == 0; },
        isOdd  = function(n) { return !isEven(n); },
        m      = transit.map([
            isEven, "isEven",
            isOdd, "isOdd"
        ]),
        s      = transit.set();

    test.equal(m.get(isEven), "isEven");
    test.equal(m.get(isOdd), "isOdd");

    var v0 = m["delete"](isEven);

    test.equals(v0, "isEven");
    test.equals(m.get(isEven, "removed"), "removed");

    s.add(isEven);
    s.add(isOdd);

    test.equal(s.size, 2);
    test.ok(s.has(isEven));
    test.ok(s.has(isOdd));

    var v1 = s["delete"](isEven);

    test.equals(v1, isEven);
    test.ok(!s.has(isEven));

    test.done();
};

// =============================================================================
// Recursive asMapKey
// =============================================================================

exports.testRecursiveAsMapKey = function(test) {
    var r = transit.reader("json"),
        expected = transit.map(["cached", 0, transit.symbol("Explain"), "cached"]);

    test.ok(transit.equals(r.read("[\"^ \",[\"~#'\",\"cached\"],0,\"~$Explain\",\"^0\"]"),
                           expected));

    test.done();
};

// =============================================================================
// Binary Data
// =============================================================================

exports.testBinaryData = function(test) {
    var s  = "[\"~#\'\",\"~bc3VyZS4=\"]",
        r0 = transit.reader("json"),
        r1 = transit.reader("json", {preferBuffers: false}),
        w  = transit.writer("json");
    test.ok(r0.read(s) instanceof Buffer);
    test.ok(transit.isBinary(r0.read(s)));
    test.equal(w.write(r0.read(s)), "[\"~#\'\",\"~bc3VyZS4=\"]");
    test.ok(r1.read(s) instanceof Uint8Array);
    test.ok(transit.isBinary(r1.read(s)));
    test.equal(w.write(r1.read(s)), "[\"~#\'\",\"~bc3VyZS4=\"]");
    test.done();
};

// =============================================================================
// Cloning
// =============================================================================

exports.testMapClone = function(test) {
    var m0 = transit.map(["foo", 1, "bar", 2]),
        m1 = m0.clone();

    test.ok(transit.equals(m0, m1), "Cloned map equals original map");

    test.done();
};

exports.testSetClone = function(test) {
    var s0 = transit.set("foo", "bar", 1),
        s1 = transit.set("foo", "bar", 1);

    test.ok(transit.equals(s0, s1.clone()));

    test.done();
};

// =============================================================================
// Tickets
// =============================================================================

// TJS-22
exports.testUndefinedHandlerKey = function(test) {
    try {
        var foo  = {},
            wrtr = transit.writer("json-verbose", {
                handlers: transit.map([
                    foo.bar, foo
                ])
            });
    } catch(e) {
        test.equal(e.message, "Cannot create handler for JavaScript undefined");
        test.done();
    }
};
|||#
