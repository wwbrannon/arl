# Standard Library: Core, R Interop, and Testing

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## Special Forms

### quote

Return expr without evaluating it. The shorthand `'expr` is equivalent.

**Signature:** `(quote expr)`

**Examples:**

``` arl
(quote (+ 1 2))  ; => (+ 1 2)
'(a b c)         ; => (a b c)
```

**See also:** [quasiquote](#quasiquote), [eval](#eval)

------------------------------------------------------------------------

### if

Evaluate then or else based on test truthiness.

**Signature:** `(if test then [else])`

**Examples:**

``` arl
(if (> 3 2) "yes" "no") ; => "yes"
(if #f 1)               ; => #nil
```

**See also:** [and](#and), [or](#or)

------------------------------------------------------------------------

### define

Bind a name in the current lexical environment.

**Signature:** `(define name value)`

**Examples:**

``` arl
(define x 42)
(define add (lambda (a b) (+ a b)))
```

**See also:** [set!](#set-bang), [lambda](#lambda)

------------------------------------------------------------------------

### set!

Update an existing binding in the current environment chain.

**Signature:** `(set! name value)`

**Examples:**

``` arl
(define x 1)
(set! x (+ x 1)) ; x => 2
```

**See also:** [define](#define)

------------------------------------------------------------------------

### lambda

Create an anonymous function with lexical scope.

**Signature:** `(lambda (params...) body...)`

**Examples:**

``` arl
((lambda (x) (+ x 1)) 41) ; => 42
```

**See also:** [define](#define)

------------------------------------------------------------------------

### begin

Evaluate expressions in sequence and return the final result.

**Signature:** `(begin expr...)`

**Examples:**

``` arl
(begin (define x 1) (set! x (+ x 1)) x) ; => 2
```

**See also:** [if](#if)

------------------------------------------------------------------------

### defmacro

Define a macro that transforms code before evaluation.

**Signature:** `(defmacro name (params...) body...)`

**Examples:**

``` arl
(defmacro when (test . body)
`(if ,test (begin ,@body) #nil))
```

**See also:** [macroexpand](#macroexpand),
[macroexpand-1](#macroexpand-1), [quasiquote](#quasiquote)

------------------------------------------------------------------------

### quasiquote

Build code/data templates with selective evaluation. The shorthand
`` `expr `` is equivalent.

**Signature:** `(quasiquote expr)`

**Examples:**

``` arl
(define x 10)
`(a ,x c) ; => (a 10 c)
```

**See also:** [quote](#quote), [unquote](#unquote),
[unquote-splicing](#unquote-splicing)

------------------------------------------------------------------------

### unquote

Evaluate expr inside a quasiquote template. Within a template, the
shorthand `,expr` is equivalent.

**Signature:** `(unquote expr)`

**Examples:**

``` arl
(define x 10)
`(a ,x c) ; => (a 10 c)
```

**See also:** [quasiquote](#quasiquote),
[unquote-splicing](#unquote-splicing)

------------------------------------------------------------------------

### unquote-splicing

Splice list elements into a quasiquoted list. Within a quasiquote
template, the shorthand `,@expr` is equivalent.

**Signature:** `(unquote-splicing expr)`

**Examples:**

``` arl
(define xs '(2 3))
`(1 ,@xs 4) ; => (1 2 3 4)
```

**See also:** [quasiquote](#quasiquote), [unquote](#unquote)

------------------------------------------------------------------------

### and

Short-circuit logical conjunction.

**Signature:** `(and expr...)`

**Examples:**

``` arl
(and #t 1 2) ; => 2
(and #f (stop "error")) ; => #f (second form not evaluated)
```

**See also:** [or](#or), [if](#if)

------------------------------------------------------------------------

### or

Short-circuit logical disjunction.

**Signature:** `(or expr...)`

**Examples:**

``` arl
(or #f #nil 0 7) ; => 7
(or #t (stop "error")) ; => #t (second form not evaluated)
```

**See also:** [and](#and), [if](#if)

------------------------------------------------------------------------

### while

Repeatedly evaluate body while condition remains truthy.

**Signature:** `(while test body...)`

**Examples:**

``` arl
(define i 0)
(while (< i 3) (set! i (+ i 1)))
i ; => 3
```

**See also:** [begin](#begin)

------------------------------------------------------------------------

### delay

Create a promise that delays evaluation of expr until forced.

**Signature:** `(delay expr)`

**Examples:**

``` arl
(define p (delay (+ 1 2)))
(force p) ; => 3
```

**See also:** [force](#force), [promise?](#promise-p),
[promise-expr](#promise-expr)

------------------------------------------------------------------------

### import

Load a module and bind it as a first-class value. By default,
`(import name)` binds the module environment to the symbol `name` for
qualified access via `name/sym`. Use `:refer` to bring specific exports
(or all exports) into scope unqualified. Use `:as` to alias the module
binding.

**Signature:**
`(import name) (import name :refer (sym1 sym2 ...)) (import name :refer :all) (import name :as alias) (import name :rename ((old new) ...)) (import name :reload) (import "path/to/file.arl")`

**Examples:**

``` arl
(import math)
(math/inc 5)               ; qualified access
(import math :refer :all)
(inc 5)                     ; unqualified access
(import math :as m)
(m/inc 5)                   ; aliased qualified access
```

**See also:** [module](#module), [load](#load),
[module-ref](#module-ref)

------------------------------------------------------------------------

### module

Define a module with explicit exports. A named module registers itself
in the module registry. A nameless module derives its name from the
source file. `export-all` exports all non-private definitions; add
`:re-export` to also re-export imported symbols.

**Signature:**
`(module name (export ...) body...) (module (export ...) body...) (module name (export-all) body...) (module name (export-all :re-export) body...)`

**Examples:**

``` arl
(module demo
(export answer)
(define answer 42))
```

**See also:** [import](#import), [module?](#module-p),
[module-exports](#module-exports), [module-name](#module-name)

------------------------------------------------------------------------

## Error and Warning

### error

Signal an error with message.

**Signature:** `(error msg)`

**Parameters:** - **`msg`** — Error message string

**Examples:**

    arl> (try-catch (error "something went wrong")
    arl>   (catch e ($ e "message")))     ; => "something went wrong"
    #> "something went wrong"
    arl> (try-catch (error "oops")
    arl>   (catch e "caught"))            ; => "caught"
    #> "caught"

**See also:** [warn](#warn), [assert](#assert),
[try-catch](https://willbrannon.com/arl/articles/lang-control.html#try-catch)

------------------------------------------------------------------------

### warn

Emit warning with message.

**Signature:** `(warn msg)`

**Parameters:** - **`msg`** — Warning message string

**Examples:**

    arl> (warn "check your input")       ; emits warning, returns #nil
    #> "check your input"

**See also:** [error](#error),
[trace](https://willbrannon.com/arl/articles/lang-strings-io.html#trace)

------------------------------------------------------------------------

## Identity and Values

### identity

Return argument.

**Signature:** `(identity x)`

**Parameters:** - **`x`** — Value to return unchanged

**Examples:**

    arl> (identity 42)             ; => 42
    #> 42
    arl> (identity "hello")        ; => "hello"
    #> "hello"
    arl> (map identity '(1 2 3))   ; => (1 2 3)
    #> (1 2 3)

**See also:**
[map](https://willbrannon.com/arl/articles/lang-functional.html#map)

------------------------------------------------------------------------

### values

Return multiple values to a call-with-values consumer.

**Signature:** `(values args...)`

**Parameters:** - **`args`** — Values to package together

**Examples:**

    arl> (values 1 2 3)            ; => multiple-values container
    #> (values 1 2 3 )

**See also:** [values?](#values-p),
[call-with-values](#call-with-values)

------------------------------------------------------------------------

### values?

Return \#t if x is a multiple-values container.

**Signature:** `(values? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (values? (values 1 2))    ; => #t
    #> TRUE
    arl> (values? 42)              ; => #f
    #> FALSE

**See also:** [values](#values), [call-with-values](#call-with-values)

------------------------------------------------------------------------

### call-with-values

Call producer and pass its values to consumer.

**Signature:** `(call-with-values producer consumer)`

**Parameters:** - **`producer`** — Zero-argument function that returns
values - **`consumer`** — Function that receives the produced values as
arguments

**Examples:**

    arl> (call-with-values
    arl>   (lambda () (values 1 2))
    arl>   (lambda (a b) (+ a b)))  ; => 3
    #> 3

**See also:** [values](#values), [values?](#values-p)

------------------------------------------------------------------------

## Function Application

### funcall

Apply a function with a provided list of arguments.

**Signature:** `(funcall fn args)`

**Parameters:** - **`fn`** — Function to apply - **`args`** — List of
arguments to pass to fn

**Examples:**

    arl> (funcall + (list 1 2 3))  ; => 6
    #> 6
    arl> (funcall c (list 1 2 3))  ; => c(1, 2, 3)
    #> 1 2 3

**See also:** apply, [r-call](#r-call)

------------------------------------------------------------------------

### r-call

Call an R function with optional environment. Searches from .GlobalEnv
by default, finding base and loaded package functions.

**Signature:** `(r-call fn [args (list])`

**Parameters:** - **`fn`** — Symbol or string naming the R function -
**`args`** — List of arguments to pass (default empty) - **`envir`** —
Environment to search for fn (default .GlobalEnv)

**Examples:**

    arl> (r-call "mean" (list (c 1 2 3)))   ; => 2
    #> 2
    arl> (r-call "ls" (list))                ; list .GlobalEnv bindings
    #> "args" "fn"
    arl> (r-call "Sys.time" (list))          ; current time
    #> 2026-04-25 00:36:39.001122

**Note:** R functions are directly available in Arl without `r-call`.
Use `r-call` when you need to look up a function by string name or
specify the search environment.

**See also:** [funcall](#funcall), [r-eval](#r-eval)

------------------------------------------------------------------------

### get

Get a binding by name, defaulting to .GlobalEnv.

**Signature:** `(get name [envir] [inherits])`

**Parameters:** - **`name`** — Symbol or string naming the binding to
look up - **`envir`** — Environment to search in (default .GlobalEnv) -
**`inherits`** — Whether to search parent environments (default \#t)

**Examples:**

    arl> (get "mean")              ; => the `mean` function
    #> <function>
    arl> (get "pi" (baseenv))      ; => 3.141593
    #> 3.14159265358979

**See also:** [r-call](#r-call)

------------------------------------------------------------------------

### unbind-variable

Remove a variable binding from an environment.

**Signature:** `(unbind-variable name [envir])`

**Parameters:** - **`name`** — Symbol or string naming the variable to
remove - **`envir`** — Environment to remove from (default current
environment)

**Examples:**

    arl> (begin
    arl>   (define tmp-var 42)
    arl>   (unbind-variable "tmp-var" (current-env))
    arl>   (try-catch tmp-var (catch e "gone")))  ; => "gone"
    #> "gone"

**See also:** [get](#get), [define](#define)

------------------------------------------------------------------------

### run

Evaluate a file in an isolated child environment. Uses `parent` as the
child environment parent. Definitions/imports in the loaded file remain
in the child and are not visible in `parent`.

**Signature:** `(run path [parent])`

**Parameters:** - **`path`** — File path to evaluate - **`parent`** —
Parent environment for the child (default current environment)

**Examples:**

    arl> (define run-demo-path (tempfile :fileext ".arl"))
    #> "/tmp/RtmprSyiNj/file21371d812370.arl"
    arl> (writeLines (c "(define run-demo-val 1)") run-demo-path)
    arl> (run run-demo-path)
    #> 1
    arl> (assert-equal "missing" (try-catch run-demo-val (catch e "missing")))
    #> TRUE
    arl> (unlink run-demo-path)
    #> 0

**See also:** [load](#load), [current-env](#current-env)

------------------------------------------------------------------------

## License

### license

Display Arl and R license information.

**Signature:** `(license)`

**Examples:**

    arl> (license)                 ; prints Arl and R license info
    #> Arl is licensed under the MIT License:
    #> 
    #> MIT License
    #> 
    #> Copyright (c) 2026 William Brannon
    #> 
    #> Permission is hereby granted, free of charge, to any person obtaining a copy
    #> of this software and associated documentation files (the "Software"), to deal
    #> in the Software without restriction, including without limitation the rights
    #> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    #> copies of the Software, and to permit persons to whom the Software is
    #> furnished to do so, subject to the following conditions:
    #> 
    #> The above copyright notice and this permission notice shall be included in all
    #> copies or substantial portions of the Software.
    #> 
    #> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    #> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    #> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    #> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    #> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    #> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    #> SOFTWARE.
    #> 
    #> 
    #> Arl is built on R. R's license:
    #> 
    #> 
    #> This software is distributed under the terms of the GNU General
    #> Public License, either Version 2, June 1991 or Version 3, June 2007.
    #> The terms of version 2 of the license are in a file called COPYING
    #> which you should have received with
    #> this software and which can be displayed by RShowDoc("COPYING").
    #> Version 3 of the license can be displayed by RShowDoc("GPL-3").
    #> 
    #> Copies of both versions 2 and 3 of the license can be found
    #> at https://www.R-project.org/Licenses/.
    #> 
    #> A small number of files (the API header files listed in
    #> R_DOC_DIR/COPYRIGHTS) are distributed under the
    #> LESSER GNU GENERAL PUBLIC LICENSE, version 2.1 or later.
    #> This can be displayed by RShowDoc("LGPL-2.1"),
    #> or obtained at the URI given.
    #> Version 3 of the license can be displayed by RShowDoc("LGPL-3").
    #> 
    #> 'Share and Enjoy.'

------------------------------------------------------------------------

## Macro Introspection

### macroexpand-1

Expand one layer of macros in expr.

**Signature:** `(macroexpand-1 expr)`

**Parameters:** - **`expr`** — Expression to macro-expand

**Examples:**

    arl> (macroexpand-1 '(when #t 42))  ; => (if #t (begin 42) #nil)
    #> (if TRUE (begin 42) .__nil)

**See also:** [macroexpand](#macroexpand),
[macroexpand-all](#macroexpand-all)

------------------------------------------------------------------------

### macroexpand-all

Fully expand all macros in expr. Same as macroexpand with no depth.

**Parameters:** - **`expr`** — Expression to recursively macro-expand

**Examples:**

    arl> (macroexpand-all '(when #t 42))  ; => (if #t (begin 42) #nil)
    #> (if TRUE (begin 42) .__nil)

**See also:** [macroexpand](#macroexpand),
[macroexpand-1](#macroexpand-1)

------------------------------------------------------------------------

## R Nonstandard Evaluation Wrappers

These macros provide Arl-friendly interfaces to R functions that use
nonstandard evaluation (NSE). They automatically quote expressions so
you can write natural Arl code without manual quoting.

### suppressWarnings

Suppress warnings generated by evaluating expr.

**Signature:** `(suppressWarnings expr)`

**Parameters:** - **`expr`** — Expression to evaluate with warnings
suppressed

**Examples:**

    arl> (suppressWarnings (as.numeric "not a number"))  ; => NA (warning suppressed)
    #> NA
    arl> (suppressWarnings (log -1))                     ; => NaN (warning suppressed)
    #> NaN

**See also:** [suppressMessages](#suppressmessages)

------------------------------------------------------------------------

### suppressMessages

Suppress messages generated by evaluating expr.

**Signature:** `(suppressMessages expr)`

**Parameters:** - **`expr`** — Expression to evaluate with messages
suppressed

**Examples:**

    arl> (suppressMessages (message "hello"))            ; => #nil (message suppressed)

**See also:** [suppressWarnings](#suppresswarnings)

------------------------------------------------------------------------

### with

Evaluate expr in the context of data (a data frame or list).

**Signature:** `(with data expr)`

**Parameters:** - **`data`** — Data frame or list providing the
evaluation context - **`expr`** — Expression evaluated with data columns
as variables

**Examples:**

    arl> (define df (data.frame :x (c 1 2 3) :y (c 4 5 6)))
    #> (:x 1 2 3 :y 4 5 6)
    arl> (with df (+ x y))         ; => c(5, 7, 9)
    #> 5 7 9

**See also:** [within](#within)

------------------------------------------------------------------------

### within

Evaluate expr within data, returning modified data.

**Signature:** `(within data expr)`

**Parameters:** - **`data`** — Data frame to modify - **`expr`** —
Expression that can assign new/modified columns

**Examples:**

    arl> (define df (data.frame :x (c 1 2 3)))
    #> (:x 1 2 3)
    arl> (within df (<- z (* x 2)))   ; returns df with new column z
    #> (:x 1 2 3 :z 2 4 6)

**See also:** [with](#with)

------------------------------------------------------------------------

### subset

Subset x using condition. Optional rest args for select, drop, etc.

**Signature:** `(subset x condition rest...)`

**Parameters:** - **`x`** — Data frame to subset - **`condition`** —
Row-selection expression evaluated in x’s context - **`rest`** —
Optional additional arguments (e.g. select)

**Examples:**

    arl> (define df (data.frame :x (c 1 2 3) :y (c 10 20 30)))
    #> (:x 1 2 3 :y 10 20 30)
    arl> (subset df (> x 1))       ; rows where x > 1
    #> (:x 2 3 :y 20 30)

**See also:** [with](#with), [within](#within)

------------------------------------------------------------------------

### transform

transform is difficult to implement - use within() or dplyr::mutate()
instead.

**Signature:** `(transform args...)`

**Parameters:** - **`args`** — Arguments (not used; always signals an
error)

**Note:** Not yet supported due to R named-argument syntax. Use `within`
or `dplyr::mutate` instead.

**See also:** [within](#within)

------------------------------------------------------------------------

### substitute

Perform substitution in an expression, or error if called with 1 arg.

**Signature:** `(substitute args...)`

**Parameters:** - **`args`** — Expression and environment for
substitution

**Note:** Single-argument `substitute` does not work in Arl due to eager
evaluation. Use macros or explicit quoting instead. Two-argument form
works normally.

**See also:** [defmacro](#defmacro)

------------------------------------------------------------------------

## Assertion Helpers

These functions provide test-style assertions that signal errors on
failure and return `#t` on success.

### assert

Assert condition or raise error.

**Signature:** `(assert cond [msg "Assertion failed"])`

**Parameters:** - **`cond`** — Condition to test - **`msg`** — Error
message if assertion fails (default “Assertion failed”)

**Examples:**

``` arl
(assert #t)               ; => #t
(assert (> 3 2))          ; => #t
(assert #f "must be true")  ; signals "must be true"
(assert #f)               ; signals "Assertion failed"
```

**See also:** [error](#error)

------------------------------------------------------------------------

### assert-equal

Assert expected and actual are equal?.

**Signature:** `(assert-equal expected actual)`

**Parameters:** - **`expected`** — Expected value - **`actual`** —
Actual value to compare

**Examples:**

``` arl
(assert-equal 3 (+ 1 2))  ; => #t
(assert-equal "a" "a")    ; => #t
(assert-equal 1 2)        ; signals error
```

**See also:** [assert-eq](#assert-eq), [assert-true](#assert-true)

------------------------------------------------------------------------

### assert-true

Assert value is truthy.

**Signature:** `(assert-true value)`

**Parameters:** - **`value`** — Value expected to be truthy

**Examples:**

``` arl
(assert-true #t)          ; => #t
(assert-true 1)           ; => #t
(assert-true #f)          ; signals error
```

**See also:** [assert-false](#assert-false), [assert](#assert)

------------------------------------------------------------------------

### assert-false

Assert value is falsy.

**Signature:** `(assert-false value)`

**Parameters:** - **`value`** — Value expected to be falsy

**Examples:**

``` arl
(assert-false #f)         ; => #t
(assert-false #nil)       ; => #t
(assert-false #t)         ; signals error
```

**See also:** [assert-true](#assert-true), [assert](#assert)

------------------------------------------------------------------------

### assert-eq

Assert expected and actual are identical (R’s
[`identical()`](https://rdrr.io/r/base/identical.html)).

**Signature:** `(assert-eq expected actual)`

**Parameters:** - **`expected`** — Expected value - **`actual`** —
Actual value to compare with identical?

**Examples:**

``` arl
(assert-eq 42 42)          ; => #t
(assert-eq "abc" "abc")    ; => #t
(assert-eq 1 1L)           ; signals error (double vs integer)
(assert-eq 1 "1")          ; signals error (different types)
```

**Note:** Uses R’s
[`identical()`](https://rdrr.io/r/base/identical.html), which checks
exact structural identity including type. `1` (double) and `1L`
(integer) are not identical. Use `assert-equal` for value comparison
with `equal?`.

**See also:** [assert-equal](#assert-equal), [assert](#assert)

------------------------------------------------------------------------

### assert-error

Assert expression throws an error.

**Signature:** `(assert-error expr)`

**Parameters:** - **`expr`** — Expression expected to signal an error

**Examples:**

``` arl
(assert-error (error "boom"))   ; => #t
(assert-error (stop "fail"))    ; => #t
(assert-error 42)               ; signals error
```

**See also:** [assert](#assert), [error](#error)

------------------------------------------------------------------------

### assert-no-error

Assert expression does not throw an error.

**Signature:** `(assert-no-error expr)`

**Parameters:** - **`expr`** — Expression expected to complete without
error

**Examples:**

``` arl
(assert-no-error 42)                ; => #t
(assert-no-error (+ 1 2))           ; => #t
(assert-no-error (error "boom"))    ; signals error
```

**See also:** [assert-error](#assert-error), [assert](#assert)

------------------------------------------------------------------------

## Arithmetic

### +

Variadic addition. With no arguments returns 0 (additive identity). With
one argument returns it unchanged. With two or more, returns their sum
left-to-right.

**Signature:** `(+ a ...)`

**Examples:**

    arl> (+)           ; => 0
    #> 0
    arl> (+ 5)         ; => 5
    #> 5
    arl> (+ 1 2 3)     ; => 6
    #> 6

**See also:** [-](#minus), [\*](#star), [/](#div)

------------------------------------------------------------------------

### \*

Variadic multiplication. With no arguments returns 1 (multiplicative
identity). With one argument returns it unchanged. With two or more,
returns their product left-to-right.

**Signature:** `(* a ...)`

**Examples:**

    arl> (*)           ; => 1
    #> 1
    arl> (* 5)         ; => 5
    #> 5
    arl> (* 2 3 4)     ; => 24
    #> 24

**See also:** [+](#plus), [-](#minus), [/](#div)

------------------------------------------------------------------------

### -

Variadic subtraction. Requires at least one argument. With one argument
returns its negation. With two or more, subtracts subsequent arguments
from the first left-to-right.

**Signature:** `(- a ...)`

**Examples:**

    arl> (- 5)         ; => -5
    #> -5
    arl> (- 10 3)      ; => 7
    #> 7
    arl> (- 10 3 2)    ; => 5
    #> 5

**See also:** [+](#plus), [\*](#star), [/](#div)

------------------------------------------------------------------------

### /

Variadic division. Requires at least one argument. With one argument
returns its reciprocal. With two or more, divides the first argument by
subsequent arguments left-to-right.

**Signature:** `(/ a ...)`

**Examples:**

    arl> (/ 5)         ; => 0.2
    #> 0.2
    arl> (/ 10 2)      ; => 5
    #> 5
    arl> (/ 100 5 2)   ; => 10
    #> 10

**See also:** [+](#plus), [-](#minus), [\*](#star)

------------------------------------------------------------------------

## Comparison

### \<

Variadic less-than. With fewer than two arguments returns \#t (vacuously
true). With two or more, checks that each argument is strictly less than
the next (chained pairwise comparison).

**Signature:** `(< a b ...)`

**Examples:**

    arl> (< 1 2)       ; => #t
    #> TRUE
    arl> (< 1 2 3)     ; => #t
    #> TRUE
    arl> (< 1 3 2)     ; => #f
    #> FALSE

**See also:** [\<=](#lte), [\>](#gt), [\>=](#gte), [=](#num-eq)

------------------------------------------------------------------------

### \<=

Variadic less-than-or-equal. With fewer than two arguments returns \#t.
With two or more, checks that each argument is less than or equal to the
next.

**Signature:** `(<= a b ...)`

**Examples:**

    arl> (<= 1 2)      ; => #t
    #> TRUE
    arl> (<= 1 1 2)    ; => #t
    #> TRUE
    arl> (<= 2 1)      ; => #f
    #> FALSE

**See also:** [\<](#lt), [\>](#gt), [\>=](#gte), [=](#num-eq)

------------------------------------------------------------------------

### \>

Variadic greater-than. With fewer than two arguments returns \#t. With
two or more, checks that each argument is strictly greater than the
next.

**Signature:** `(> a b ...)`

**Examples:**

    arl> (> 3 2)       ; => #t
    #> TRUE
    arl> (> 3 2 1)     ; => #t
    #> TRUE
    arl> (> 3 1 2)     ; => #f
    #> FALSE

**See also:** [\>=](#gte), [\<](#lt), [\<=](#lte), [=](#num-eq)

------------------------------------------------------------------------

### \>=

Variadic greater-than-or-equal. With fewer than two arguments returns
\#t. With two or more, checks that each argument is greater than or
equal to the next.

**Signature:** `(>= a b ...)`

**Examples:**

    arl> (>= 3 2)      ; => #t
    #> TRUE
    arl> (>= 2 2 1)    ; => #t
    #> TRUE
    arl> (>= 1 2)      ; => #f
    #> FALSE

**See also:** [\>](#gt), [\<](#lt), [\<=](#lte), [=](#num-eq)

------------------------------------------------------------------------

### =

Variadic equality comparison (NULL-safe). With fewer than two arguments
returns \#t. With two or more, checks that all adjacent pairs are equal.
NULL comparisons follow Scheme semantics: (= \#nil \#nil) is \#t, (=
\#nil x) is \#f.

**Signature:** `(= a b ...)`

**Examples:**

    arl> (= 1 1)       ; => #t
    #> TRUE
    arl> (= 1 1 1)     ; => #t
    #> TRUE
    arl> (= 1 2)       ; => #f
    #> FALSE
    arl> (= "a" "a")   ; => #t
    #> TRUE

**Note:** Alias for ==. Both = and == are NULL-safe variadic equality.

**See also:** [==](#num-eq-eq), [!=](#bang-eq), [\<](#lt), [\>](#gt)

------------------------------------------------------------------------

### ==

Variadic equality comparison (NULL-safe). Identical to =. With fewer
than two arguments returns \#t. With two or more, checks that all
adjacent pairs are equal.

**Signature:** `(== a b ...)`

**Examples:**

    arl> (== 1 1)      ; => #t
    #> TRUE
    arl> (== 1 1 1)    ; => #t
    #> TRUE
    arl> (== 1 2)      ; => #f
    #> FALSE

**See also:** [=](#num-eq), [!=](#bang-eq), [\<](#lt), [\>](#gt)

------------------------------------------------------------------------

### !=

Inequality comparison (NULL-safe). Returns \#t if a and b are not equal.

**Signature:** `(!= a b)`

**Examples:**

    arl> (!= 1 2)      ; => #t
    #> TRUE
    arl> (!= 1 1)      ; => #f
    #> FALSE
    arl> (!= #nil #nil) ; => #f
    #> FALSE

**See also:** [=](#num-eq), [==](#num-eq-eq)

------------------------------------------------------------------------

### not

Logical negation using Arl truthiness. Returns \#t if x is falsy (0,
\#f, or \#nil), \#f otherwise.

**Signature:** `(not x)`

**Examples:**

    arl> (not #f)      ; => #t
    #> TRUE
    arl> (not #nil)    ; => #t
    #> TRUE
    arl> (not 0)       ; => #t
    #> TRUE
    arl> (not 1)       ; => #f
    #> FALSE
    arl> (not "hi")    ; => #f
    #> FALSE

**See also:** [and](#and), [or](#or)

------------------------------------------------------------------------

## Evaluation

### eval

Evaluate a Arl expression in the current environment.

**Signature:** `(eval expr)`

**Examples:**

    arl> (eval '(+ 1 2))           ; => 3
    #> 3
    arl> (eval '(list 1 2 3))      ; => (1 2 3)
    #> (1 2 3)

**See also:** [r-eval](#r-eval), [read](#read),
[macroexpand](#macroexpand)

------------------------------------------------------------------------

### read

Parse a string into a Arl expression without evaluating it.

**Signature:** `(read source)`

**Examples:**

    arl> (read "(+ 1 2)")          ; => (+ 1 2) (unevaluated)
    #> (+ 1 2)
    arl> (read "42")               ; => 42
    #> 42
    arl> (read "foo")              ; => foo (symbol)
    #> foo

**Note:** Returns the first expression from the source string, or \#nil
if the string contains no expressions. Like R’s parse(), but returns Arl
S-expressions.

**See also:** [eval](#eval),
[read-from-string](https://willbrannon.com/arl/articles/lang-strings-io.html#read-from-string)

------------------------------------------------------------------------

### write

Convert a Arl expression to its string representation. The inverse of
read.

**Signature:** `(write expr)`

**Examples:**

    arl> (write '(+ 1 2))          ; => "(+ 1 2)"
    #> "(+ 1 2)"
    arl> (write 42)                ; => "42"
    #> "42"
    arl> (write "hello")           ; => "\"hello\""
    #> ""hello""
    arl> (write #t)                ; => "#t"
    #> "#t"

**Note:** Produces output that can be parsed back with read, ensuring
the round-trip property: (read (write expr)) equals expr for any
expression produced by read.

**See also:** [read](#read), [eval](#eval),
[format-value](https://willbrannon.com/arl/articles/lang-strings-io.html#format-value)

------------------------------------------------------------------------

### load

Load and evaluate an Arl source file in the target environment. Defaults
to the current environment.

**Signature:** `(load path [env])`

**Examples:**

    arl> (define __load_demo_path (tempfile :fileext ".arl"))
    #> "/tmp/RtmprSyiNj/file21374d264d05.arl"
    arl> (writeLines (c "(define __load_demo_value 42)") __load_demo_path)
    arl> (load __load_demo_path)
    #> 42
    arl> __load_demo_value
    #> 42
    arl> (define __load_demo_env (new.env :parent (current-env)))
    #> <environment>
    arl> (load __load_demo_path __load_demo_env)
    #> 42
    arl> (get "__load_demo_value" __load_demo_env)
    #> 42
    arl> (unlink __load_demo_path)
    #> 0

**See also:** [eval](#eval), [read](#read), [run](#run),
[current-env](#current-env)

------------------------------------------------------------------------

### r-eval

Evaluate an R expression directly via R’s eval(), bypassing Arl’s
compiler.

**Signature:** `(r-eval expr)`

**Examples:**

    arl> (r-eval (quote (seq_len 5)))  ; => c(1, 2, 3, 4, 5)
    #> 1 2 3 4 5

**Note:** Useful for evaluating raw R calls that use R control flow
(for, while) or other constructs that Arl normally overrides.

**See also:** [eval](#eval), [r-call](#r-call)

------------------------------------------------------------------------

## Documentation

### help

Show help for a topic. Topic may be a symbol or string. Use :package to
force R help lookup from a specific package.

**Signature:** `(help topic [:package pkg])`

**Examples:**

    arl> (help if)
    #> Topic: if
    #> Usage: (if test then [else])
    #> Description: Evaluate then or else based on test truthiness.
    #> 
    #> Examples:
    #>   (if (> 3 2) "yes" "no") ; => "yes"
    #>   (if #f 1)               ; => #nil
    #> 
    #> See also: and, or
    arl> (help "sum")
    #> Topic: sum
    #> Usage:      sum(..., na.rm = FALSE)
    #> Description:      'sum' returns the sum of all the values present in its arguments.
    #> 
    #> Arguments:
    #>        ...: numeric or complex or logical vectors.
    #>      na.rm: logical ('TRUE' or 'FALSE').  Should missing values
    #>             (including 'NaN') be removed?
    #> 
    #> Examples:
    #>        ## Pass a vector to sum, and it will add the elements together.
    #>        sum(1:5)
    #>        ## Pass several numbers to sum, and it also adds the elements.
    #>        sum(1, 2, 3, 4, 5)
    #>        ## In fact, you can pass vectors into several arguments, and everything gets added.
    #>        sum(1:2, 3:5)
    #>        ## If there are missing values, the sum is unknown, i.e., also missing, ....
    #>        sum(1:5, NA)
    #>        ## ... unless  we exclude missing values explicitly:
    #>        sum(1:5, NA, na.rm = TRUE)
    #> 
    #> Note: R package: base
    #> 
    #> See also:      'colSums' for row and column sums.
    arl> (help "writeLines" :package "base")
    #> Topic: writeLines
    #> Usage:      writeLines(text, con = stdout(), sep = "\n", useBytes = FALSE)
    #> Description:      Write text lines to a connection.
    #> 
    #> Arguments:
    #>       text: a character vector.
    #>        con: a connection object or a character string.
    #>        sep: character string.  A string to be written to the connection
    #>             after each line of text.
    #>   useBytes: logical.  See 'Details'.
    #> 
    #> Note: R package: base
    #> 
    #> See also:      'connections', 'writeChar', 'writeBin', 'readLines', 'cat'

**See also:** [doc](#doc), [macroexpand](#macroexpand)

------------------------------------------------------------------------

### doc!

Attach documentation fields to a function. With a single string
argument, sets the description (backward compatible). With keyword
arguments, sets specific fields and merges with existing documentation.

**Signature:**
`(doc! fn "docstring") or (doc! fn :description "..." :examples "...")`

**Examples:**

``` arl
(doc! my-fn "Doubles the input.")
(doc! my-fn :examples "(my-fn 3) ; => 6")
(doc! my-fn :description "Doubles." :note "Fast path.")
```

**See also:** [doc](#doc), [help](#help)

------------------------------------------------------------------------

### doc

Retrieve documentation from a function. With no field argument, returns
the description. Pass a field name string to get a specific field, or
“all” to get the full documentation list.

**Signature:** `(doc fn) or (doc fn "field")`

**Examples:**

``` arl
(doc my-fn)              ; => "Doubles the input."
(doc my-fn "examples")   ; => "(my-fn 3) ; => 6"
(doc my-fn "all")        ; => named list of all fields
```

**See also:** [doc!](#doc-bang), [help](#help)

------------------------------------------------------------------------

## Macro Utilities

### capture

Mark a symbol for intentional capture in a macro body, overriding
hygiene.

**Signature:** `(capture 'sym expr)`

**Examples:**

    arl> (defmacro aif (test then alt)
    arl> `(let ((it ,test))
    arl> (if it ,(capture 'it then) ,(capture 'it alt))))
    arl> (aif (+ 2 3) it 0)          ; => 5
    #> 5

**Note:** Use capture when writing anaphoric macros or other macros that
intentionally introduce a binding visible to the caller. Without
capture, Arl’s automatic hygiene renames macro-introduced symbols to
prevent accidental capture.

**See also:** [gensym](#gensym), [macroexpand](#macroexpand),
[defmacro](#defmacro)

------------------------------------------------------------------------

### gensym

Generate a unique uninterned symbol, useful for writing hygienic macros.

**Signature:** `(gensym [prefix])`

**Examples:**

    arl> (gensym)                  ; => G1 (unique symbol)
    #> G__159
    arl> (gensym "tmp")            ; => tmp2 (unique with prefix)
    #> tmp__160

**Note:** Each call returns a fresh symbol guaranteed not to conflict
with user-defined names. The optional prefix defaults to “G”.

**See also:** [macroexpand](#macroexpand),
[macroexpand-1](#macroexpand-1)

------------------------------------------------------------------------

### macro?

Return \#t if the symbol names a currently-defined macro.

**Signature:** `(macro? sym)`

**Examples:**

    arl> (defmacro my-mac (x) x)
    arl> (macro? 'my-mac)          ; => #t
    #> TRUE
    arl> (macro? 'car)             ; => #f
    #> FALSE

------------------------------------------------------------------------

### macroexpand

Recursively expand all macros in expr until no macro calls remain.

**Signature:** `(macroexpand expr)`

**Examples:**

    arl> (defmacro my-when (test body)
    arl> `(if ,test ,body #nil))
    arl> (macroexpand '(my-when #t 42))  ; => (if #t 42 #nil)
    #> (if TRUE 42 .__nil)

**Note:** Also available as `macroexpand-all` (alias).

**See also:** [macroexpand-1](#macroexpand-1), [macro?](#macro-p),
[gensym](#gensym)

------------------------------------------------------------------------

## Promises (Lazy Evaluation)

### promise?

Return \#t if x is a promise (created with `delay`).

**Signature:** `(promise? x)`

**Examples:**

    arl> (define p (delay (+ 1 2)))
    #> <promise>
    arl> (promise? p)              ; => #t
    #> TRUE
    arl> (promise? 42)             ; => #f
    #> FALSE

**See also:** [force](#force), [promise-expr](#promise-expr)

------------------------------------------------------------------------

### force

Force a promise, evaluating its delayed expression and returning the
result. If x is not a promise, returns x unchanged.

**Signature:** `(force x)`

**Examples:**

    arl> (define p (delay (+ 1 2)))
    #> <promise>
    arl> (force p)                 ; => 3
    #> 3
    arl> (force 42)                ; => 42 (non-promise passed through)
    #> 42

**See also:** [promise?](#promise-p), [promise-expr](#promise-expr),
[delay](#delay)

------------------------------------------------------------------------

### promise-expr

Extract the unevaluated expression from a promise.

**Signature:** `(promise-expr p)`

**Examples:**

    arl> (define p (delay (+ 1 2)))
    #> <promise>
    arl> (promise-expr p)          ; => (+ 1 2)
    #> (+ 1 2)

**Note:** Signals an error if p is not a promise.

**See also:** [promise?](#promise-p), [force](#force), [delay](#delay)

------------------------------------------------------------------------

## Environment Introspection

### toplevel-env

Return the top-level engine environment where user definitions and
standard library exports live. This is the environment used by eval_text
and the REPL. Note that builtins-env sits between this environment and
R’s baseenv() in the parent chain.

**Signature:** `(toplevel-env)`

**Examples:**

    arl> (environment? (toplevel-env))  ; => #t
    #> TRUE

**See also:** [current-env](#current-env), [builtins-env](#builtins-env)

------------------------------------------------------------------------

### builtins-env

Return the builtins environment that sits between the top-level engine
environment and R’s baseenv(). Contains core operators (=, +, \*, \<,
etc.), module/macro registries, and environment commands. Module
environments parent to this environment rather than to toplevel-env,
which enforces that modules must explicitly import stdlib functions.

**Signature:** `(builtins-env)`

**Examples:**

    arl> (environment? (builtins-env))  ; => #t
    #> TRUE

**See also:** [toplevel-env](#toplevel-env), [current-env](#current-env)

------------------------------------------------------------------------

### current-env

Return the current Arl evaluation environment.

**Signature:** `(current-env)`

**Examples:**

    arl> (environment? (current-env))  ; => #t
    #> TRUE

**See also:** [toplevel-env](#toplevel-env),
[builtins-env](#builtins-env)

------------------------------------------------------------------------

## Module Introspection

### module-ref

Look up a symbol in a module or namespace. This is the desugared form of
qualified access: `mod/sym` parses as `(module-ref mod sym)`.

**Signature:** `(module-ref mod sym)`

**Examples:**

    arl> (import math)
    arl> (module-ref math inc)  ; same as math/inc
    #> <function>

**See also:** [import](#import), [module?](#module-p),
[module-exports](#module-exports)

------------------------------------------------------------------------

### module?

Return \#t if x is a module environment (registered in the module
registry).

**Signature:** `(module? x)`

**Examples:**

    arl> (import math)
    arl> (module? math)  ; => #t
    #> TRUE
    arl> (module? 42)    ; => #f
    #> FALSE

**See also:** [namespace?](#namespace-p),
[module-exports](#module-exports), [module-name](#module-name)

------------------------------------------------------------------------

### namespace?

Return \#t if x is a namespace node (created by importing a hierarchical
module name like `a/b`).

**Signature:** `(namespace? x)`

**Examples:**

    arl> (namespace? 42)      ; => #f
    #> FALSE
    arl> (namespace? "hello") ; => #f
    #> FALSE

**See also:** [module?](#module-p), [module-ref](#module-ref)

------------------------------------------------------------------------

### module-exports

Return the list of exported symbol names from a module.

**Signature:** `(module-exports mod)`

**Examples:**

    arl> (import math)
    arl> (module-exports math)  ; => list of exported names
    #> ("%" "inc" "dec" "clamp" "within?" "signum" "expt" "quotient" "remainder" "modulo" "gcd" "lcm" "make-rectangular" "make-polar" "real-part" "imag-part" "magnitude" "angle")

**See also:** [module?](#module-p), [module-name](#module-name),
[module-ref](#module-ref)

------------------------------------------------------------------------

### module-name

Return the canonical name string of a module.

**Signature:** `(module-name mod)`

**Examples:**

    arl> (import math)
    arl> (module-name math)  ; => "math"
    #> "math"

**See also:** [module?](#module-p), [module-exports](#module-exports)

------------------------------------------------------------------------
