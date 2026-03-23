# Getting Started

Arl is a Lisp dialect that compiles to R. Every R function and data
structure is available directly, and the macro system lets you transform
code at compile time. This vignette walks through installing Arl,
running the REPL, and writing your first expressions.

## Installation

``` r
# install.packages("arl")
devtools::install_github("wwbrannon/arl")
```

Arl is not on CRAN at the time of writing; install from GitHub with
`devtools` as shown above. Once a CRAN release is available, the package
can also be installed with the built-in `install.packages` function. If
you have a local clone of the repository,
[`devtools::install()`](https://devtools.r-lib.org/reference/install.html)
from the repo directory will also work.

## Start the REPL

``` r
engine <- arl::Engine$new()
engine$repl()
```

The engine loads the prelude automatically (though this [can be
customized](#loading-stdlib-modules)), so functions like `when`, `let`,
`map`, and `->` are available immediately.

At the prompt, you can enter Arl expressions:

    arl> (+ 1 2)
    #> 3
    arl> (define x 10)
    #> 10
    arl> (* x 2)
    #> 20

Type `(quit)` or press Ctrl+C to exit the REPL.

**REPL options:** You can control the REPL with options before calling
`engine$repl()`: `arl.repl_quiet` (minimal banner),
`arl.repl_use_history` (set to `FALSE` to avoid touching R’s readline
history), and `arl.repl_bracketed_paste` (enable/disable bracketed paste
mode for cleaner multiline pastes). See the discussion of [runtime
options](https://willbrannon.com/arl/articles/runtime-options.md) for
more.

## Run from the command line

Run
[`arl::install_cli()`](https://willbrannon.com/arl/reference/install_cli.md)
to see how to put the CLI wrapper on your PATH:

``` r
arl::install_cli()
#> Arl CLI wrapper script: /path/to/arl/bin/posix/arl
#>
#> To make it available on your PATH, create a symlink:
#>
#>   mkdir -p ~/.local/bin
#>   ln -s "/path/to/arl/bin/posix/arl" ~/.local/bin/arl
#>
#> Then ensure ~/.local/bin is on your PATH.
```

Then you can evaluate code without opening R:

``` bash
arl --eval "(+ 1 2)"
arl --file script.arl
arl -q                    # quiet REPL (minimal banner)
arl --help                # see all options
```

You can also pass multiple files; they run in order in a shared engine,
so definitions from earlier files are visible to later ones. Use
`--no-stdlib` to start a bare engine without stdlib modules.

## Evaluate Arl from R

The simplest way to evaluate Arl inside an R script is `eval_text` (or
its alias `eval_string`), which reads and evaluates in one step:

``` r
engine <- arl::Engine$new()
engine$eval_text("(define x 10) (+ x 5)")
engine$eval_string("(define y 20) (+ y 5)")
```

For finer control, you can parse and evaluate separately:

``` r
exprs <- engine$read("(define x 10) (+ x 5)")
engine$eval(exprs[[1]], exprs[[2]])
```

[`eval()`](https://rdrr.io/r/base/eval.html) accepts multiple
expressions and evaluates them sequentially, returning the last value.

## Passing R data to the engine

Use `$define()` to inject R objects into the engine so Arl code can use
them:

``` r
engine <- arl::Engine$new()
engine$define("my_data", mtcars)
engine$eval_text("(head my_data 3)")
```

Calls to `$define()` return the engine invisibly, so they can be
chained:

``` r
engine$define("x", 10)$define("y", 20)$eval_text("(+ x y)")
```

To read results back into R, use `$eval_text()` (which returns the last
value) or `$get_env()` to access the engine’s environment directly.

## Run Arl files

From the REPL, use `load` to run a file in the current environment so
that its definitions are visible:

``` arl
(load "script.arl")
```

Use `run` to execute a file in an isolated child environment
(definitions are not visible to later code):

``` arl
(run "script.arl")
```

From R, you can mirror the two forms with `load_file_in_env`:

``` r
engine <- Engine$new()

# Like (load ...): definitions visible in the engine
engine$load_file_in_env("script.arl")

# Like (run ...): isolated; definitions not visible
engine$load_file_in_env("script.arl", new.env(parent = engine$get_env()))
```

If you don’t specify an environment, `load_file_in_env` uses the
engine’s toplevel environment.

## Loading stdlib modules

The engine loads the **prelude** automatically — 10 modules (`logic`,
`core`, `types`, `list`, `equality`, `functional`, `control`,
`sequences`, `binding`, `threading`) whose exports are available
immediately. Functions like `when`, `let`, `->`, and `try` come from the
prelude and need no import. `do-list` requires
`(import looping :refer :all)`.

Non-prelude modules must be loaded explicitly with `import`. A bare
`(import X)` makes the module available for qualified access (e.g.
`math/inc`); add `:refer :all` to also import all exports unqualified:

``` arl
(import math)                  ; math/inc, math/dec, ...
(import math :refer :all)      ; inc/dec/abs/clamp/... (unqualified)
(import looping :refer :all)   ; loop/recur/until
(import sort :refer :all)      ; list-sort/sort-by/...
(import strings :refer :all)   ; string-split/string-join/...
```

Imports can also be selective or aliased:

``` arl
(import control :refer (when unless))
(import strings :as str)       ; str/string-split, str/string-join, ...
```

Prelude modules are already available, but you can still import them
explicitly in your own modules (which start with an empty scope plus
prelude access):

``` arl
(import control :refer :all)   ; when/unless/cond/case/try/try-catch/call-cc
```

See [Modules and
Imports](https://willbrannon.com/arl/articles/modules.md) for details on
creating and importing modules, and the [Language
Reference](https://willbrannon.com/arl/articles/lang-reference.md) for a
list of stdlib modules and the full set of functions/macros each
provides.

## R functions are available directly

Because Arl compiles to R and its environment chain parents to R’s
[`baseenv()`](https://rdrr.io/r/base/environment.html), every function
in R’s base package is available without any import. Functions like
`max`, `min`, `sum`, `c`, `length`, `paste`, `lapply`, and hundreds of
others work as-is:

    arl> (max 1 5 3)
    #> 5
    arl> (length (c 10 20 30))
    #> 3
    arl> (paste "hello" "world" :sep ", ")
    #> "hello, world"

Arl shadows some R names with its own versions (arithmetic operators are
variadic, comparisons chain, `=` is equality not assignment). R’s
default packages (`stats`, `utils`, `grDevices`, `graphics`, `datasets`,
`methods`) are also attached automatically, so functions like `median`,
`head`, `lm`, and data like `iris` work without a prefix. See [Inherited
R
Functions](https://willbrannon.com/arl/articles/lang-reference.html#inherited-r-functions)
in the Language Reference for details, and [R Interop and Data
Workflows](https://willbrannon.com/arl/articles/r-interop.md) for
calling R packages, using keyword arguments, and `r-eval`.

## Core syntax and semantics

### Truthiness

Arl follows R’s truthiness rules: `#f`/`FALSE`, `#nil`/`NULL`, and `0`
are falsy; everything else is truthy. This differs from Scheme, where
only `#f` is falsy. See
[Troubleshooting](https://willbrannon.com/arl/articles/troubleshooting.md)
for common pitfalls.

### Definitions and functions

    arl> ; Define a variable
    arl> (define greeting "hello")
    #> "hello"

    arl> ; Define a function
    arl> (define factorial
    arl>   (lambda (n)
    arl>     (if (< n 2)
    arl>       1
    arl>       (* n (factorial (- n 1))))))
    #> <function>

    arl> (factorial 5)
    #> 120

Use `unbind-variable` (which ultimately relies on R’s `rm`) to remove a
binding:

    arl> (define tmp 42)
    #> 42
    arl> (unbind-variable 'tmp (current-env))

### Local bindings

    arl> ; let binds variables in a local scope
    arl> (let ((x 10)
    arl>       (y 20))
    arl>   (+ x y))
    #> 30

### Conditionals

    arl> ; if is the basic conditional
    arl> (if (> 3 2) "yes" "no")
    #> "yes"

    arl> ; cond handles multiple branches
    arl> ; the fallback #t case is like "else"
    arl> (define describe
    arl>   (lambda (n)
    arl>     (cond
    arl>       ((< n 0) "negative")
    arl>       ((= n 0) "zero")
    arl>       (#t "positive"))))
    #> <function>

    arl> (describe 5)
    #> "positive"

    arl> ; when is a one-armed conditional (no else branch)
    arl> (when (> 3 2)
    arl>   (print "3 is greater"))
    #> [1] "3 is greater"
    #> "3 is greater"

### Sequencing

    arl> ; begin evaluates expressions in order
    arl> ; the last value is returned
    arl> (begin
    arl>   (define a 1)
    arl>   (define b 2)
    arl>   (+ a b))
    #> 3

### Lists and quoting

Code and data in Arl, as in Lisp generally, are made of lists. You can
define and manipulate lists:

    arl> ; Comments start with semicolon

    arl> (list 1 2 3)
    #> (1 2 3)
    arl> (car (list 1 2 3))  ; car = first element
    #> 1
    arl> (cdr (list 1 2 3))  ; cdr = rest of list after car
    #> (2 3)

Lists typed in at the prompt are, by default, evaluated. To prevent
evaluation, use the special form `quote` or its syntactic sugar `'`:

    arl> (quote (+ 1 2))    ; => unevaluated expression (R language object / call)
    #> (+ 1 2)
    arl> '(+ 1 2)           ; simple alias for (quote ...)
    #> (+ 1 2)

    arl> (list? '(+ 1 2))
    #> TRUE
    arl> (base::is.list '(+ 1 2))
    #> FALSE

This is an important Arl-vs-R distinction: `list?` follows Arl’s
Lisp-style semantics and treats quoted forms (which are [R call/language
objects](https://cran.r-project.org/doc/manuals/r-devel/R-lang.html#Computing-on-the-language))
as lists, while R’s [`base::is.list`](https://rdrr.io/r/base/list.html)
reports the underlying R object type and returns `FALSE` for calls.

You can also perform selective evaluation with the `quasiquote` template
syntax (written with the backtick `` ` ``), which is widely used in
defining [macros](https://willbrannon.com/arl/articles/macros.md):

    arl> ; Quasiquote allows selective evaluation with , and ,@
    arl> (define x 10)
    #> 10
    arl> (define y (list 1 2 3))
    #> (1 2 3)

    arl> `(list ,x 20 30)   ; => (list 10 20 30) -- x is substituted, rest is literal
    #> (list 10 20 30)

    arl> `(list ,@y 20 30)   ; => (list 1 2 3 20 30) -- y is spliced in at same level
    #> (list 1 2 3 20 30)

For convenience, an Arl list is also an R list under the hood:

    arl> (base::is.list (list 1 2 3))
    #> TRUE

Lisp’s traditional pair lists made of cons cells are also supported,
though they are implemented differently and less commonly used:

    arl> (define pl (3 . (4 . 5))) ; this is a dotted-pair list
    #> (3 4 . 5)
    arl> (pair? pl)
    #> TRUE
    arl> (list? pl) ; pair lists are a different kind of object
    #> FALSE

For a concise guide to R-style lists vs dotted pairs and when to use
each, see [Pairlists vs R
lists](https://willbrannon.com/arl/articles/arl-vs-scheme.html#pairlists-vs-r-lists).
For more on quoted forms as Arl lists but R language objects, see
[Pairlists vs R
lists](https://willbrannon.com/arl/articles/arl-vs-scheme.html#pairlists-vs-r-lists)
and [R eval and
calls](https://willbrannon.com/arl/articles/r-interop.html#r-eval).

For more details on the standard library and macros, see the other
vignettes in this package.

## Getting help

Arl has a built-in help system. Use `help` with a string to look up any
special form, macro, built-in, or stdlib function:

``` arl
(help "define")   ; special form
(help "when")     ; macro -- shows docs and usage
(help "map")      ; stdlib function
```

For functions or macros with documentation, `help` shows the signature,
description, examples, and cross-references automatically. It also falls
through to R’s built-in help for R functions.

### Documenting your functions

Arl provides two mechanisms for attaching documentation to functions and
macros:

- **`;;'` annotation comments** — Place roxygen-like tags
  (`@description`, `@examples`, `@seealso`, `@note`, etc) immediately
  before a `define` or `defmacro`. The compiler bakes the documentation
  in at compile time with no runtime overhead. This is the recommended
  approach for source files.

- **`doc!`** — Attach or update documentation at runtime, useful for
  interactive work: `(doc! my-fn "Description here.")` or with keyword
  arguments like `(doc! my-fn :examples "(my-fn 3)")`.

Both produce the same `arl_doc` attribute, so `help` and `doc` work
identically regardless of how documentation was attached. See
[Documenting Functions and
Macros](https://willbrannon.com/arl/articles/documenting-functions-macros.md)
for the full reference.

## Next steps

- [Functions](https://willbrannon.com/arl/articles/functions.md)
- [Modules and Imports](https://willbrannon.com/arl/articles/modules.md)
- [Macros and
  Quasiquote](https://willbrannon.com/arl/articles/macros.md)
- [R Interop and Data
  Workflows](https://willbrannon.com/arl/articles/r-interop.md)
- [Examples](https://willbrannon.com/arl/articles/examples.md)
- [Troubleshooting](https://willbrannon.com/arl/articles/troubleshooting.md)
- [Arl Compared to
  Scheme](https://willbrannon.com/arl/articles/arl-vs-scheme.md)
