# rmatch

When you work in Common Lisp, you eventually discover
destructuring-bind and then you find it indispensable. If you then try
some other lisp variant which doesn't have destructuring-bind (e.g.,
scheme) you will miss it and look for alternatives. In the case of
Racket, that alternative is provided by the
[racket/match](https://docs.racket-lang.org/reference/match.html)
library, which includes several advanced pattern matching
forms. Ironically, now I find myself missing these forms whenever I
work in Common Lisp. So this package is an attempt to recreate some of
the Racket-style pattern matching forms in Common Lisp, at least to a
first-order approximation. It's implemented by building on Paul
Graham's pattern matching macros from "On Lisp".

# Usage

So far, `match-let`, `match-let*` and `defun/match` have been implemented.

~~~lisp
 (match-let ((pat seq) ...) body)
~~~

 All symbols in all `pat` expressions are bound to their corresponding `seq` values and available to `body`.

Example:

~~~lisp
 (match-let (((a b) '(10 20))
             ((x y) '(30 40)))
   (list a b x y))

=> (10 20 30 40)
~~~

~~~lisp
 (match-let* ((pat seq) ...) body)
~~~

Similar to `match-let` except that symbols bound in each `pat` expression are available to subsequent `seq` expressions.

Example:

~~~lisp
(let ((b 100))
 (match-let* (((a b) (list b 20))
              ((x y) (list 30 b)))
    (list a b x y)))

=> (100 20 30 20)
~~~

~~~lisp
 (defun/match fname (args)
    ((seq body) ...))
~~~

Creates a `defun` with the name `fname`. The body of the function
attempts to match `args` against each `seq` expression. As soon as a
match is found, the corresponding `body` is evaluated.

Example:

~~~lisp
 (defun/match factorial (n)
   ((0) 1)
   ((_) (* n (factorial (- n 1)))))

 (factorial 10)

=> 3628800
~~~

~~~lisp
 (match pat ((seq body) ...)
~~~

Attempts to match `pat` against each `seq` expression. As soon as a match
is found, the corresponding `body` is evaluated. `seq` expressions do not need
to have the same number of patterns as `pat`.

Example:

~~~lisp
 (match (list 1 (list 2) 2)
   ((a b) "two")
   ((a (b) b) "repeated")
   ((a (b) c) (list c b a)))

=> "repeated"

 (match (list 1 (list 2) 3)
   ((a b) "two")
   ((a (b) b) "repeated")
   ((a (b) c) (list c b a)))

=> (3 2 1)

 (match '(2 3)
   ((a b) "two")
   ((a (b) b) "repeated")
   ((a (b) c) (list c b a)))

=> "two"
~~~
