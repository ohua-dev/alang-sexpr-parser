# Parses simple S-Expressions into ALang


An example for a standalone ohua module using the lisp-like syntax

```clojure
(ns some_ns)
    
    
(require [ohua.math [add isZero]])

(defalgo sqare [x]
    (add x x))

(defalgo algo1 [someParam]
    (let [a (square someParam)
          coll0 (ohua.lang/smap (fn [i] (square i)) coll)]
    (if (isZero a)
        coll0
        a)))

(defalgo main [param]
    (algo0 param))
```
