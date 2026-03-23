# Functions

This vignette covers how to define, call, and compose functions in Arl.

## Defining Functions

Functions are created with `lambda` and bound to names with `define`:

    arl> (define double
    arl>   (lambda (x)
    arl>     (* x 2)))
    #> <function>

    arl> (double 5)
    #> 10

**Important:** Unlike Scheme, `(define (f x) body)` is **not** function
shorthand in Arl. Because `define` supports
[destructuring](#destructuring), writing `(define (f x) body)` tries to
destructure the value `body` into the pattern `(f x)` — it does not
create a function. Always use the explicit `define` + `lambda` form
shown above.

``` arl
;; WRONG — this is destructuring, not function definition
(define (f x) (+ x 1))

;; CORRECT
(define f (lambda (x) (+ x 1)))
```

## Parameter Features

Arl’s `lambda` supports several parameter styles. These work identically
in `defmacro` parameters.

### Required parameters

    arl> (define add (lambda (a b) (+ a b)))
    #> <function>
    arl> (add 3 4)
    #> 7

### Optional parameters with defaults

Wrap a parameter in a pair `(name default)`:

    arl> (import strings :refer (string-append))

    arl> (define greet
    arl>   (lambda ((name "world"))
    arl>     (string-append "hello, " name)))
    #> <function>

    arl> (greet)          ; uses default
    #> "hello, world"
    arl> (greet "Alice")  ; overrides default
    #> "hello, Alice"

### Rest parameters

Use `.` to collect remaining arguments into a list:

    arl> (define sum-all
    arl>   (lambda (first . rest)
    arl>     (reduce + (cons first rest))))
    #> <function>

    arl> (sum-all 1 2 3 4)
    #> 10

### Destructuring parameters

Use `(pattern ...)` to destructure an argument:

    arl> (define first-of-pair
    arl>   (lambda ((pattern (a b)))
    arl>     a))
    #> <function>

    arl> (first-of-pair (list 10 20))
    #> 10

Patterns can be nested, have defaults, or combine with rest parameters:

    arl> (define point-sum
    arl>   (lambda ((pattern (x y) (list 0 0)))
    arl>     (+ x y)))
    #> <function>

    arl> (point-sum)            ; uses default (0 0)
    #> 0
    arl> (point-sum (list 3 4)) ; => 7
    #> 7

### Combining parameter styles

You can mix required, optional, destructuring, and rest parameters:

    arl> (define flexible
    arl>   (lambda (required (opt 10) . rest)
    arl>     (list required opt rest)))
    #> <function>

    arl> (flexible 1)         ; => (1 10 ())
    #> (1 10 ())
    arl> (flexible 1 2 3 4)   ; => (1 2 (3 4))
    #> (1 2 (3 4))

## Destructuring

Destructuring lets you unpack a data structure into individual variables
in a single `define` statement. Instead of binding a name to a value,
you provide a **pattern** — a nested list of names — and Arl matches
each name to the corresponding element of the value:

    arl> (define (x y z) (list 10 20 30))
    #> (10 20 30)
    arl> (list x y z)
    #> (10 20 30)

This works with nested structures too:

    arl> (define (p (q r)) (list 1 (list 2 3)))
    #> (1 (2 3))
    arl> (list p q r)
    #> (1 2 3)

### `destructuring-bind`

The `destructuring-bind` macro provides a scoped form of destructuring.
It binds a pattern to a value and evaluates body forms with those
bindings in scope:

    arl> (destructuring-bind (first second . rest) (list 1 2 3 4 5)
    arl>   (list first second rest))
    #> (1 2 (3 4 5))

This is what macros like `let*` and `when-let` use under the hood — each
binding in a `let` form is a destructuring bind, so patterns work
anywhere a `let` binding does:

    arl> (let (((a b) (list 1 2))
    arl>       ((c d) (list 3 4)))
    arl>   (+ a b c d))
    #> 10

### Destructuring in function parameters

As shown in [Parameter Features](#parameter-features) above, `lambda`
parameters can also destructure their arguments using the
`(pattern ...)` syntax. See [Destructuring
parameters](#destructuring-parameters) for examples.

## Calling Functions

### Positional arguments

    arl> (define add (lambda (a b) (+ a b)))
    #> <function>
    arl> (add 3 4)
    #> 7

### Keyword arguments

Keywords (`:name value`) pass named arguments. This is especially useful
when calling R functions:

    arl> (seq :from 1 :to 5)
    #> 1 2 3 4 5

Keywords also work with Arl-defined functions — the keyword name is
matched to the parameter name:

    arl> (define make-point
    arl>   (lambda (x y)
    arl>     (list x y)))
    #> <function>

    arl> (make-point :y 20 :x 10)
    #> (10 20)

See [R
Interop](https://willbrannon.com/arl/articles/r-interop.html#keyword-syntax)
for details on keyword syntax and quoting.

## Local Functions

Use `let`, `let*`, and `letrec` to bind functions in local scope.

### `let` / `let*` for simple local functions

    arl> (let ((double (lambda (x) (* x 2)))
    arl>       (inc    (lambda (x) (+ x 1))))
    arl>   (double (inc 3)))
    #> 8

### `letrec` for recursive local functions

`letrec` allows bindings to refer to each other, which is necessary for
local recursive or mutually-recursive functions:

    arl> (letrec ((even? (lambda (n)
    arl>                   (if (= n 0) #t (odd? (- n 1)))))
    arl>          (odd?  (lambda (n)
    arl>                   (if (= n 0) #f (even? (- n 1))))))
    arl>   (list (even? 10) (odd? 7)))
    #> (TRUE TRUE)

Because `letrec` expands into `set!`, self-recursive `letrec` lambdas
are [automatically tail-call
optimized](https://willbrannon.com/arl/articles/tail-call-optimization.md).
(Note that mutually recursive functions are not!)

## Higher-Order Functions

Arl’s standard library provides the usual higher-order toolkit:

    arl> (map (lambda (x) (* x x)) (list 1 2 3 4))
    #> (1 4 9 16)
    arl> (filter even? (list 1 2 3 4 5 6))
    #> (2 4 6)

    arl> (define add5 (partial + 5))
    #> <function>
    arl> (add5 10)
    #> 15

    arl> (define abs-then-double
    arl>   (compose (lambda (x) (* x 2)) abs))
    #> <function>
    arl> (abs-then-double -3)
    #> 6

See [Standard Library: Higher-Order
Functions](https://willbrannon.com/arl/articles/lang-functional.md) for
the full reference including `reduce`, `curry`, `juxt`, `memoize`, and
more.

## Recursion

### Self-TCO (automatic)

When you define a named function that calls itself in tail position, the
compiler automatically rewrites it as a loop — no stack overflow:

    arl> (define factorial
    arl>   (lambda (n acc)
    arl>     (if (< n 2)
    arl>       acc
    arl>       (factorial (- n 1) (* acc n)))))
    #> <function>

    arl> ;; No stack overflow (but 100000! is too large
    arl> ;; to be representable and overflows to Inf)
    arl> (factorial 100000 1)
    #> Inf

### `loop` / `recur`

For explicit looping or patterns where self-TCO does not apply, use
`loop`/`recur`:

    arl> (import looping :refer (loop recur))

    arl> (loop ((i 5) (acc 1))
    arl>   (if (< i 2)
    arl>     acc
    arl>     (recur (- i 1) (* acc i))))
    #> 120

See [Tail Call
Optimization](https://willbrannon.com/arl/articles/tail-call-optimization.md)
for details on what counts as tail position and how the optimization
works.

## Macros vs Functions

Macros use the same parameter syntax (required, optional, rest,
destructuring) but operate on **unevaluated syntax** at compile time
rather than on runtime values. To define a macro, use `defmacro` instead
of `define` and `lambda`:

``` arl
;; Function: runs at eval time, receives evaluated args, returns new value
(define double (lambda (x) (* x 2)))

;; Macro: runs at compile time, receives unevaluated syntax, returns new syntax
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))
```

See [Macros and
Quasiquote](https://willbrannon.com/arl/articles/macros.md) for the full
guide.

## Related guides

- [Getting
  Started](https://willbrannon.com/arl/articles/getting-started.md)
- [Macros and
  Quasiquote](https://willbrannon.com/arl/articles/macros.md)
- [Tail Call
  Optimization](https://willbrannon.com/arl/articles/tail-call-optimization.md)
- [Standard Library: Higher-Order
  Functions](https://willbrannon.com/arl/articles/lang-functional.md)
- [R Interop and Data
  Workflows](https://willbrannon.com/arl/articles/r-interop.md)
- [Arl Compared to
  Scheme](https://willbrannon.com/arl/articles/arl-vs-scheme.md)
