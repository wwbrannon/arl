# R Interop and Data Workflows

Arl can call any R function directly. Because Arl code is compiled to R
and the environment chain includes R’s
[`baseenv()`](https://rdrr.io/r/base/environment.html) and exports from
R’s default packages (`stats`, `utils`, `grDevices`, `graphics`,
`datasets`, `methods`), functions like `max`, `sum`, `median`, `head`,
`lm`, and data like `iris` all work without imports or package prefixes.
See [Inherited R
Functions](https://willbrannon.com/arl/articles/lang-reference.html#inherited-r-functions)
in the Language Reference for a survey of commonly used inherited
functions and details on which names Arl shadows with its own versions.

## Calling R functions

    arl> (mean (c 1 2 3 4 5))
    #> 3
    arl> (seq :from 1 :to 10 :by 2)
    #> 1 3 5 7 9

Keywords (`:from`, `:to`) map to named arguments in R.

### Keyword syntax

Keywords starting with `:` serve a dual purpose in Arl:

1.  **As named argument syntax**: Bare keywords in function calls become
    named arguments

        arl> (seq :from 1 :to 10)  ; R: seq(from = 1, to = 10)
        #> 1 2 3 4 5 6 7 8 9 10

2.  **As self-evaluating values**: Keywords are first-class values that
    can be bound to variables

        arl> :keyword          ; evaluates to a keyword object
        #> "keyword"
        arl> (define k :foo)   ; bind a keyword to a variable
        #> "foo"

To pass a keyword as a *value* (rather than as named argument syntax),
quote it:

``` arl
(equal? ':foo ':bar)     ; compares two keyword values → #f
(equal? ':foo ':foo)     ; → #t

(equal? :foo :bar)       ; error: bare keywords become named arguments
```

This design allows keywords to work naturally with R’s named arguments
while remaining first-class values when needed.

### r-call

Most R functions can be called directly from Arl (e.g. `(mean x)`).
`r-call` is useful in two situations: (1) when an Arl binding has
**shadowed** an R function name and you need to reach the original, and
(2) when the function name is **computed at runtime** (stored in a
variable as a string). It takes a character string, skips all
Arl-defined evaluation frames, and looks up the name in the containing R
scopes:

    arl> ; shadow the binding so direct calls fail
    arl> (define sum (lambda () (stop "error")))
    #> <function>

    arl> ; r-call can still find it
    arl> (r-call "sum" (list 1 2 3))
    #> 6

    arl> ; avoid keeping this shadowing binding around (see below)
    arl> (unbind-variable 'sum (current-env))

### r-eval

`r-eval` evaluates an R expression directly via R’s
[`eval()`](https://rdrr.io/r/base/eval.html), bypassing Arl’s compiler
entirely. Where `r-call` looks up a function by name and calls it with
pre-evaluated arguments, `r-eval` takes an unevaluated call expression
and hands it to R as-is:

    arl> (r-eval (quote (seq_len 5)))  ; => c(1, 2, 3, 4, 5)
    #> 1 2 3 4 5

Note: these quoted forms are R language/call objects. Arl’s `list?`
treats them as lists for Lisp semantics, while R’s
[`base::is.list`](https://rdrr.io/r/base/list.html) reports `FALSE`.

Use Arl’s `call` function to build the expression programmatically, and
pass `(current-env)` so R can see your local bindings:

    arl> (define n 3)
    #> 3
    arl> (r-eval (call (list 'seq_len 'n)) (current-env))  ; => c(1, 2, 3)
    #> 1 2 3

`r-eval` is the escape hatch for R constructs that Arl normally
overrides (`for`, `while`) or that require non-standard evaluation (see
below).

### Non-standard evaluation

Some R functions use non-standard evaluation (NSE) — they capture
unevaluated expressions and evaluate them in a controlled context.
Examples include `tryCatch`, `with`, `within`, and `subset`.

`r-call` can’t help here because it evaluates arguments **before**
passing them to the R function (via `do.call`). `r-eval` avoids this:
you build an unevaluated call expression and let R evaluate it, so the
NSE function receives exactly the expression it expects.

``` arl
;; WRONG - thunk is evaluated before tryCatch can catch errors
(r-call "tryCatch"
  (list :expr (thunk) :error (lambda (e) #t)))

;; CORRECT - build unevaluated expression, then eval it
(define thunk-expr (call (list thunk)))     ; unevaluated call
(r-eval (call (list 'tryCatch thunk-expr    ; 'tryCatch = quoted symbol
                    :error (lambda (e) #t)))
        (current-env))
```

**Key points:**

1.  Use Arl’s `call` function to build unevaluated call expressions
2.  Use a quoted symbol (`'tryCatch`) not a string (`"tryCatch"`)
3.  Use `r-eval` (not `r-call`) to evaluate the constructed expression
4.  Pass `(current-env)` as the evaluation environment

This pattern is used throughout the stdlib — see `try` in `control.arl`
and `assert-error` in `assert.arl` for working examples.

#### Detecting errors with a sentinel value

When wrapping error-prone code, you need to distinguish “the error
handler ran” from “the code succeeded and happened to return the same
value”. Use `gensym` to create a unique sentinel:

``` arl
(define error-caught (gensym "error-caught"))  ; unique symbol
(define result
  (r-eval (call (list 'tryCatch thunk-expr
                      :error (lambda (e) error-caught)))
          (current-env)))
;; Check if error-caught is identical to result
(if (identical? result error-caught)
  "error was caught"
  "code ran successfully")
```

### substitute

R’s `substitute(expr)` (the single-argument form) captures the
unevaluated expression passed to a function. This doesn’t work in Arl:
Arl evaluates arguments before calling functions, so there is no
unevaluated expression to capture. Use macros instead — a macro’s body
receives its arguments as unevaluated syntax, which is the same thing
`substitute` provides in R:

    arl> ;; In R you might write:
    arl> ;;   log_call <- function(expr) { cat(deparse(substitute(expr))); expr }
    arl> ;;
    arl> ;; In Arl, use a macro:
    arl> (defmacro log-call (expr)
    arl>   `(begin
    arl>      (cat (deparse ',expr) "\n")
    arl>      ,expr))

    arl> (log-call (+ 1 2))
    #> 1 + 2 
    #> 3

The two-argument form `(substitute expr env)` **does** work — it
performs substitution on `expr` using bindings from `env`, which doesn’t
require capturing the caller’s expression:

    arl> (substitute '(+ a b) (list :a 1 :b 2))  ; => (+ 1 2)
    #> (+ 1 2)

## Working with formulas

    arl> (lm (~ mpg cyl) :data mtcars)
    #> 
    #> Call:
    #> lm(formula = mpg ~ cyl, data = mtcars)
    #> 
    #> Coefficients:
    #> (Intercept)          cyl  
    #>      37.885       -2.876  

The `~` operator builds an R formula without evaluating its arguments.

## Accessing R objects

    arl> (define mylist (list :a 1 :b 2))
    #> (:a 1 :b 2)
    arl> ($ mylist "a") ; => 1
    #> 1

    arl> (define vector (c 1 2 3))
    #> 1 2 3
    arl> ([ vector 1) ; => 1
    #> 1

Use `::` and `:::` to access R packages:

    arl> (:: stats median)
    #> <function>
    arl> (::: stats .lm.fit)
    #> <function>

R base operators are also available directly:

    arl> (^ 2 3)
    #> 8
    arl> (%/% 7 3)
    #> 2

### Managing bindings

Use `get` to retrieve a binding by name and `unbind-variable` to remove
one. Both accept an optional environment argument (defaulting to
`.GlobalEnv` for `get` and `(current-env)` for `unbind-variable`):

    arl> (define temp-binding 42)
    #> 42
    arl> (get 'temp-binding (current-env))     ; => 42
    #> 42
    arl> (unbind-variable 'temp-binding (current-env))
    arl> ; temp-binding is no longer defined

Names can also be passed as strings:

``` arl
(unbind-variable "temp-binding" (current-env))
```

### Numeric edge cases

Because Arl delegates to R for numeric operations, edge cases follow R
semantics (not Scheme’s). For example, division by zero yields `Inf`,
and `NaN` comparisons propagate `NA`:

    arl> (/ 1 0)         ; => Inf
    #> Inf
    arl> (== NaN NaN)    ; => NA
    #> NA

## Passing data between R and Arl

Use `$define()` to inject R objects into the engine:

``` r
engine <- Engine$new()
engine$define("my_data", mtcars)
engine$eval_text("(nrow my_data)")  # => 32
```

To read results back into R, use `$eval_text()` (returns the last value)
or access the engine’s environment with `$get_env()`:

``` r
result <- engine$eval_text("(median ($ my_data \"mpg\"))")

env <- engine$get_env()
ls(env)  # see all bindings
```

## Example workflow

The [data analysis
example](https://willbrannon.com/arl/articles/examples.html#data-analysis)
shows a small analysis pipeline from Arl that uses `data.frame`, `mean`,
and `lm`. You can run it with:

``` r
engine <- Engine$new()
engine$load_file_in_env(system.file("examples", "data-analysis.arl", package = "arl"))
```

## Standard library helpers

The stdlib exposes helpers like `map`, `filter`, `reduce`, and threading
macros (`->`, `->>`). See the [Language
Reference](https://willbrannon.com/arl/articles/lang-reference.md) for
the full list.

## Related guides

- [Getting
  Started](https://willbrannon.com/arl/articles/getting-started.md)
- [Macros and
  Quasiquote](https://willbrannon.com/arl/articles/macros.md)
- [Language
  Reference](https://willbrannon.com/arl/articles/lang-reference.md)
