TODO
====

## Issues

Do we need better than ms granularity on timestamps?

arbitrary decimal PRINTING DOES NOT APPEAR TO WORK ON LW6 OR CCL.  Check rep call.

Just what is a cmap?  Have we got the tag method defn right?  scalar-p

When reading floats should we use `*read-default-float-format*` to read doubles?

Implement links.

Implement scalar extensions: `#\x`

Is it an issue that rep and tag and reader etc. are not in one
place...?  Well, we do an example of defining a scalar and composite
extension and then it's easy...

Check we are indeed following the read and write flow charts...

Does the read caching logic cope with escaped strings?  Looks like the
Read flow chart is wrong, if it's a cache code we read it from cache
and then just raturn it, rather than parsing it??

`parse-string` now has the `map-as-array-tag-p` test which diverges
from their read flow chart...

Better names for constants...

Writing floats using ~F or can/should we use exp notation for floats?
Check this is good enough, optional
`(find-method #'yason:encode '() (mapcar #'find-class '(float)))`

Check for places where I have assumed strings have at least one char...

Support "disable write caching"?

What is going on with `"{\"~#'\":\"~f-1.1E-1\"}"` and
`"[\"^ \",\"~/t\",null]"`?

Fix this mess:
(remove-method #'yason:encode (find-method #'yason:encode '() (mapcar #'find-class '(float))))
and the associated float printing issues.
