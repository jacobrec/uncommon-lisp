# uncommon-lisp
A set of macros to make common lisp pretty without losing expressiveness

## defn/fn
fn is a replacement for lambda, and defn is a replacement for
defun. Both of these act like what they replace, except they support
define inside, as well, they eliminate the need for funcall.

Examples:
```lisp
(defn test1 (a c)
  (define b 1)
  (define x (fn (x) (* 2 x)))
  (mdefine (d e) (floor 5 4))
  (x (a b c d e)))
(assert (= 8 (test1 #'+ 1)))

(defn test3 ()
  (mdefine (s c) (values #'sin #'cos))
  (+ (s 0) (c 0)))
(assert (= 1 (test3)))
```

## Better function names
Don't understand what `terpri` means, and your tired of having to
write out `multiple-value-bind` all over the place? Me too.

- `println &rest args`: prints items space seperated, with a newline at end
- `mlet`: alias for `multiple-value-bind`
