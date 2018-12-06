# Parses simple S-Expressions into ALang

[![Build Status](https://travis-ci.org/ohua-dev/alang-sexpr-parser.svg?branch=master)](https://travis-ci.org/ohua-dev/alang-sexpr-parser)

An example for a standalone ohua module using the lisp-like syntax

```clojure
(ns some_ns)


(require sf [ohua.math [add isZero]])
(require algo [some.sn [myalgo]])

(defalgo sqare [x]
    (add x x))

(defalgo algo1 [someParam]
    (let [a (square (myalgo someParam)
          coll0 (ohua.lang/smap (fn [i] (square i)) coll)]
    (if (isZero a)
        coll0
        a)))

(defalgo main [param]
    (algo0 param))
```
