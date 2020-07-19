# uncommon-lisp
A set of macros to make common lisp pretty without losing expressiveness

```lisp
;;;; End to end tests
(defn test1 (a c)
  (define b 1)
  (define x (fn (x) (* 2 x)))
  (x (a b c)))
(assert (= 4 (test1 #'+ 1)))
```
