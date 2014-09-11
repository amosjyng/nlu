Natural Language Understander
=============================

This is a natural language parser for the Scone project. To get it running,

1. Install [Quicklisp](http://beta.quicklisp.org/quicklisp.lisp)
2. In the terminal, run `sbcl --load initialize.lisp`
3. Once SBCL has finished loading, run `(nlu "Please pick up a large screwdriver and screw in the bolts.")`

Currently, due to context-switching, the parse takes about half a minute. Upon completion, you should see the output

```
(:COMMAND
 (:IN-ORDER
  (:ACTION {pick_up.v.01}
   (:OBJECT (:GENERIC {screwdriver.n.01} (:ATTRIBUTES {large.a.01}))))
  (:ACTION {screw.v.03} (:OBJECT (:SPECIFIC (:MULTIPLE {bolt.n.06}))))))
```
