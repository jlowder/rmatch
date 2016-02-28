# rmatch

When you work in Common Lisp, you eventually discover
destructuring-bind and then you find it indispensable. If you then try
some other lisp variant which doesn't have destructuring-bind (e.g.,
scheme) you will miss it and look for alternatives. In the case of
Racket, that alternative is provided by the _racket/match_ library,
which includes several advanced pattern matching forms. Ironically,
now I find myself missing these forms whenever I work in Common
Lisp. So this package is an attempt to recreate some of the
Racket-style pattern matching forms in Common Lisp, by building on the
Paul Graham's pattern matching macros from "On Lisp".

# Usage

So far, _match-let_, _match-let*_ and _defun/match_ have been implemented.

~~~lisp
 (match-let ((pat seq) ...) body)
~~~

 All symbols in all _pat_ expressions are bound to their corresponding _seq_ values and available to _body_.

Example:

~~~lisp
 (match-let (((a b) '(10 20))
             ((x y) '(30 40)))
   (list a b x y))

=> (10 20 30 40)


 (match-let* ((pat seq) ...) body)
~~~

Similar to _match-let_ except that symbols bound in each _pat_ expression are available to subsequent _seq_ expressions.

Example;

~~~lisp
(let ((b 100))
 (match-let* (((a b) (list b 20))
              ((x y) (list 30 b)))
    (list a b x y)))

=> (100 20 30 20)


 (defun/match fname (args)
    ((seq body) ...))
~~~

Creates a _defun_ with the name _fname_. The body of the function
attempt to match _args_ against each _seq_ expression. As soon as a
match is found, the corresponding _body_ is evaluated.

Example:

~~~lisp
 (defun/match factorial (n)
   ((0) 1)
   ((_) (* n (factorial (- n 1)))))

 (factorial 10)

=> 3628800
~~~
