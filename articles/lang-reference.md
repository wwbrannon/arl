# Language Reference

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing modules](#importing-modules).*

## Special forms

**Special forms** are expressions with evaluation rules that differ from
normal function calls – for example, `if` does not evaluate all its
arguments, and `define` binds a name rather than passing it as a value.
They are handled directly by the compiler and cannot be redefined or
passed as values.

- [`quote`](https://willbrannon.com/arl/articles/lang-core.html#quote) –
  Return expr without evaluating it. The shorthand `'expr` is
  equivalent.
- [`if`](https://willbrannon.com/arl/articles/lang-core.html#if) –
  Evaluate then or else based on test truthiness.
- [`define`](https://willbrannon.com/arl/articles/lang-core.html#define)
  – Bind a name in the current lexical environment.
- [`set!`](https://willbrannon.com/arl/articles/lang-core.html#set-bang)
  – Update an existing binding in the current environment chain.
- [`lambda`](https://willbrannon.com/arl/articles/lang-core.html#lambda)
  – Create an anonymous function with lexical scope.
- [`begin`](https://willbrannon.com/arl/articles/lang-core.html#begin) –
  Evaluate expressions in sequence and return the final result.
- [`defmacro`](https://willbrannon.com/arl/articles/lang-core.html#defmacro)
  – Define a macro that transforms code before evaluation.
- [`quasiquote`](https://willbrannon.com/arl/articles/lang-core.html#quasiquote)
  – Build code/data templates with selective evaluation. The shorthand
  `` `expr `` is equivalent.
- [`unquote`](https://willbrannon.com/arl/articles/lang-core.html#unquote)
  – Evaluate expr inside a quasiquote template. Within a template, the
  shorthand `,expr` is equivalent.
- [`unquote-splicing`](https://willbrannon.com/arl/articles/lang-core.html#unquote-splicing)
  – Splice list elements into a quasiquoted list. Within a quasiquote
  template, the shorthand `,@expr` is equivalent.
- [`and`](https://willbrannon.com/arl/articles/lang-core.html#and) –
  Short-circuit logical conjunction.
- [`or`](https://willbrannon.com/arl/articles/lang-core.html#or) –
  Short-circuit logical disjunction.
- [`while`](https://willbrannon.com/arl/articles/lang-core.html#while) –
  Repeatedly evaluate body while condition remains truthy.
- [`delay`](https://willbrannon.com/arl/articles/lang-core.html#delay) –
  Create a promise that delays evaluation of expr until forced.
- [`import`](https://willbrannon.com/arl/articles/lang-core.html#import)
  – Load a module and bind it as a first-class value. By default,
  `(import name)` binds the module environment to the symbol `name` for
  qualified access via `name/sym`. Use `:refer` to bring specific
  exports (or all exports) into scope unqualified. Use `:as` to alias
  the module binding.
- [`module`](https://willbrannon.com/arl/articles/lang-core.html#module)
  – Define a module with explicit exports. A named module registers
  itself in the module registry. A nameless module derives its name from
  the source file. `export-all` exports all non-private definitions; add
  `:re-export` to also re-export imported symbols.

Anything not in this list is a function or macro, whether
[built-in](#built-in-functions), [standard library](#standard-library),
user-defined, or inherited from R. Unlike special forms, these are
ordinary values and can be passed around, stored in variables, and so
on.

## Built-in functions

Certain **built-in functions** are implemented in R
([`R/engine.R`](https://github.com/wwbrannon/arl/blob/main/R/engine.R))
rather than in Arl source modules. These are low-level primitives that
need direct access to engine internals — cons-cell operations, the macro
expander, the evaluator, promise handling, and documentation helpers.
They are always available, even when the stdlib modules are not loaded
(`Engine$new(load_prelude = FALSE)`).

| Category                   | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|----------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Arithmetic                 | [`+`](https://willbrannon.com/arl/articles/lang-core.html#plus), [`*`](https://willbrannon.com/arl/articles/lang-core.html#star), [`-`](https://willbrannon.com/arl/articles/lang-core.html#minus), [`/`](https://willbrannon.com/arl/articles/lang-core.html#div)                                                                                                                                                                                                                                                                              |
| Comparison                 | [`<`](https://willbrannon.com/arl/articles/lang-core.html#lt), [`<=`](https://willbrannon.com/arl/articles/lang-core.html#lte), [`>`](https://willbrannon.com/arl/articles/lang-core.html#gt), [`>=`](https://willbrannon.com/arl/articles/lang-core.html#gte), [`=`](https://willbrannon.com/arl/articles/lang-core.html#num-eq), [`==`](https://willbrannon.com/arl/articles/lang-core.html#num-eq-eq), [`!=`](https://willbrannon.com/arl/articles/lang-core.html#bang-eq), [`not`](https://willbrannon.com/arl/articles/lang-core.html#not) |
| List and Pair Predicates   | [`pair?`](https://willbrannon.com/arl/articles/lang-types.html#pair-p)                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| List Operations            | [`car`](https://willbrannon.com/arl/articles/lang-list-seq.html#car), [`cdr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdr), [`cons`](https://willbrannon.com/arl/articles/lang-list-seq.html#cons)                                                                                                                                                                                                                                                                                                                              |
| Evaluation                 | [`eval`](https://willbrannon.com/arl/articles/lang-core.html#eval), [`read`](https://willbrannon.com/arl/articles/lang-core.html#read), [`write`](https://willbrannon.com/arl/articles/lang-core.html#write), [`load`](https://willbrannon.com/arl/articles/lang-core.html#load), [`r-eval`](https://willbrannon.com/arl/articles/lang-core.html#r-eval)                                                                                                                                                                                        |
| Documentation              | [`help`](https://willbrannon.com/arl/articles/lang-core.html#help), [`doc!`](https://willbrannon.com/arl/articles/lang-core.html#doc-bang), [`doc`](https://willbrannon.com/arl/articles/lang-core.html#doc)                                                                                                                                                                                                                                                                                                                                    |
| Macro Utilities            | [`capture`](https://willbrannon.com/arl/articles/lang-core.html#capture), [`gensym`](https://willbrannon.com/arl/articles/lang-core.html#gensym), [`macro?`](https://willbrannon.com/arl/articles/lang-core.html#macro-p), [`macroexpand`](https://willbrannon.com/arl/articles/lang-core.html#macroexpand)                                                                                                                                                                                                                                     |
| Promises (Lazy Evaluation) | [`promise?`](https://willbrannon.com/arl/articles/lang-core.html#promise-p), [`force`](https://willbrannon.com/arl/articles/lang-core.html#force), [`promise-expr`](https://willbrannon.com/arl/articles/lang-core.html#promise-expr)                                                                                                                                                                                                                                                                                                           |
| Environment Introspection  | [`toplevel-env`](https://willbrannon.com/arl/articles/lang-core.html#toplevel-env), [`builtins-env`](https://willbrannon.com/arl/articles/lang-core.html#builtins-env), [`current-env`](https://willbrannon.com/arl/articles/lang-core.html#current-env)                                                                                                                                                                                                                                                                                        |
| Module Introspection       | [`module-ref`](https://willbrannon.com/arl/articles/lang-core.html#module-ref), [`module?`](https://willbrannon.com/arl/articles/lang-core.html#module-p), [`namespace?`](https://willbrannon.com/arl/articles/lang-core.html#namespace-p), [`module-exports`](https://willbrannon.com/arl/articles/lang-core.html#module-exports), [`module-name`](https://willbrannon.com/arl/articles/lang-core.html#module-name)                                                                                                                            |

These builtins are documented alongside the stdlib functions they relate
to in the individual reference pages below.

## Inherited R functions

Because Arl compiles to R and its environment chain ultimately parents
to R’s [`baseenv()`](https://rdrr.io/r/base/environment.html), every
function in base R is available in Arl without any import or special
syntax. This is not just interop glue — many common operations you will
use day-to-day come directly from R rather than from Arl’s own builtins
or stdlib.

### What “inherited” means

When you write `(max 1 2 3)` in Arl, the compiler emits an R call to
[`max()`](https://rdrr.io/r/base/Extremes.html). There is no Arl wrapper
— R’s own `max` function runs directly. The same is true for hundreds of
base R functions. They work because R’s
[`baseenv()`](https://rdrr.io/r/base/environment.html) sits at the
bottom of the environment chain, so any name not shadowed by an Arl
builtin, stdlib export, or user definition resolves to R’s version.

### Examples of commonly used inherited functions

Here are some examples of base R functions that are used routinely in
Arl code and are **not** redefined — R’s own implementations run
directly:

| Category        | Examples                                                                                                            |
|-----------------|---------------------------------------------------------------------------------------------------------------------|
| Math            | `max`, `min`, `sum`, `prod`                                                                                         |
| Vectors         | `c`, `length`, `seq`, `seq_len`, `seq_along`, `rep`, `rev`, `unique`, `which`                                       |
| Predicates      | `is.null`, `is.na`, `is.numeric`, `is.character`, `is.logical`, `is.function`, `is.list`, `is.environment`          |
| Coercion        | `as.numeric`, `as.character`, `as.logical`, `as.integer`, `as.double`, `as.list`                                    |
| Strings         | `paste`, `paste0`, `sprintf`, `nchar`, `substr`, `sub`, `gsub`, `grepl`, `toupper`, `tolower`, `trimws`, `strsplit` |
| Data structures | `list`, `vector`, `matrix`, `data.frame`, `names`, `attr`, `attributes`                                             |
| Accessors       | `$`, `[`, `[[`, `@`                                                                                                 |
| Apply family    | `lapply`, `sapply`, `vapply`, `mapply`, `tapply`, `do.call`                                                         |
| I/O             | `cat`, `message`, `warning`, `stop`, `readLines`, `writeLines`, `readRDS`, `saveRDS`                                |
| Environment     | `environment`, `new.env`, `parent.env`, `exists`, `assign`, `ls`, `rm`                                              |

This is far from exhaustive — any function in R’s base package works in
Arl the same way.

### When Arl shadows R

Arl intentionally redefines some R names with its own versions. The most
important are the **operators**:

- **Arithmetic** (`+`, `-`, `*`, `/`): Arl’s versions are variadic, so
  `(+ 1 2 3 4)` works. R’s `+` is binary.
- **Comparison** (`<`, `<=`, `>`, `>=`): Arl’s versions chain, so
  `(< 1 2 3)` means “1 \< 2 and 2 \< 3”. R’s `<` compares two vectors.
- **Equality** (`=`, `==`, `!=`): Arl’s versions are NULL-safe and
  variadic. R’s `=` is assignment, not comparison.
- **Logical** (`!`): Arl uses `not` and the special forms `and`/`or`.
- **Control flow** (`if`, `while`, `for`): These are Arl special forms
  or macros with Lisp-style syntax.

The stdlib also shadows some base R function names with Lisp-flavored
versions:

- **Math wrappers** (`abs`, `sqrt`, `exp`, `log`, `floor`, `ceiling`,
  `round`): Arl’s versions are thin wrappers that add documentation and
  integrate with the help system; behavior is the same.
- **List operations** (`append`, `sort`, `reverse`): Arl’s versions work
  on both R lists and cons-cell pair lists, with Lisp-style semantics.
- **I/O and display** (`print`, `format`, `system`): Arl’s versions add
  Lisp-style formatting or Arl-specific behavior.
- **Other** (`get`, `identity`, `subset`, `transform`, `try`): Arl
  provides its own implementations of these with Arl-specific semantics.

When you need R’s original, use the `base::` namespace prefix:

``` arl
(base::sort (c 3 1 2))      ; R's vector sort, not Arl's list sort
(base::identity (list 1 2))  ; R's identity, not Arl's
```

### Beyond base: R’s default packages

Arl’s environment chain parents to R’s
[`baseenv()`](https://rdrr.io/r/base/environment.html), **not** to
`.GlobalEnv`. But R’s default packages — `stats`, `utils`, `grDevices`,
`graphics`, `datasets`, and `methods` — are also attached at engine
startup. Their exports are copied into a chain of environments between
`builtins_env` and
[`baseenv()`](https://rdrr.io/r/base/environment.html), mirroring how R
itself structures its search path.

This means functions like `median`, `head`, `lm`, `plot`, `rgb`, and
data like `iris` and `mtcars` work without any prefix:

``` arl
(median (c 1 2 3 4 5))
(head mtcars 3)
(lm (~ mpg cyl) :data mtcars)
```

The set of attached packages is controlled by R’s `defaultPackages`
option (see [`?options`](https://rdrr.io/r/base/options.html)). Users
can customize it by setting the `R_DEFAULT_PACKAGES` environment
variable before starting R — for example, `R_DEFAULT_PACKAGES=""`
disables all default packages, leaving only
[`baseenv()`](https://rdrr.io/r/base/environment.html).

For packages **not** in the default set, use the `::` prefix:

``` arl
(jsonlite::fromJSON "{\"a\": 1}")
(httr::GET "https://example.com")
```

See [R Interop and Data
Workflows](https://willbrannon.com/arl/articles/r-interop.md) for more
on calling R functions, using keyword arguments, formulas, and `r-eval`.

## Standard library

In addition to built-in functions, Arl has a **standard library**
written in Arl (`inst/arl/*.arl`). These stdlib modules provide various
features: list operations, math, strings, control flow, and everything
else. Modules are loaded in dependency order (each module declares its
dependencies with `(import ...)` and is loaded after the modules it
imports).

For the full, per-function reference, see the individual stdlib
reference pages:

- [Standard Library: Core, R Interop, and
  Testing](https://willbrannon.com/arl/articles/lang-core.md)
- [Standard Library: Types, Equality, and
  Conversions](https://willbrannon.com/arl/articles/lang-types.md)
- [Standard Library: Control Flow and
  Macros](https://willbrannon.com/arl/articles/lang-control.md)
- [Standard Library: Lists and
  Sequences](https://willbrannon.com/arl/articles/lang-list-seq.md)
- [Standard Library: Strings, Display, and
  I/O](https://willbrannon.com/arl/articles/lang-strings-io.md)
- [Standard Library: Collections and Data
  Structures](https://willbrannon.com/arl/articles/lang-collections.md)
- [Standard Library: Higher-Order
  Functions](https://willbrannon.com/arl/articles/lang-functional.md)
- [Standard Library: Math and Numeric
  Functions](https://willbrannon.com/arl/articles/lang-math.md)

## Importing modules

Prelude modules are loaded automatically by `Engine$new()`. Non-prelude
modules (like `math`, `looping`, `sort`, `strings`, `dict`, `set`, `io`,
etc.) require explicit `(import ...)`. The `import` form is also needed
inside your own modules (where you start with an empty scope) and when
working with a bare engine (`Engine$new(load_prelude = FALSE)`):

``` arl
; Import non-prelude modules
(import math)      ; inc/dec/abs/min/max/floor/ceiling/round/square/...
(import looping)   ; do-list/loop/recur/until
(import sort)      ; sort/sort-by
(import strings)   ; str/string-join/string-split/...
```

From R, you can create an engine with the stdlib already loaded:

``` r
engine <- Engine$new()                   # prelude loaded
bare <- Engine$new(load_prelude=FALSE)    # builtins only
```

## [Core, R Interop, and Testing](https://willbrannon.com/arl/articles/lang-core.md)

Evaluation, macro utilities, R interop helpers, and assertions.

[`license`](https://willbrannon.com/arl/articles/lang-core.html#license),
[`error`](https://willbrannon.com/arl/articles/lang-core.html#error),
[`warn`](https://willbrannon.com/arl/articles/lang-core.html#warn),
[`identity`](https://willbrannon.com/arl/articles/lang-core.html#identity),
[`values`](https://willbrannon.com/arl/articles/lang-core.html#values),
[`values?`](https://willbrannon.com/arl/articles/lang-core.html#values-p),
[`call-with-values`](https://willbrannon.com/arl/articles/lang-core.html#call-with-values),
[`funcall`](https://willbrannon.com/arl/articles/lang-core.html#funcall),
[`r-call`](https://willbrannon.com/arl/articles/lang-core.html#r-call),
[`get`](https://willbrannon.com/arl/articles/lang-core.html#get),
[`unbind-variable`](https://willbrannon.com/arl/articles/lang-core.html#unbind-variable),
[`run`](https://willbrannon.com/arl/articles/lang-core.html#run),
[`macroexpand-1`](https://willbrannon.com/arl/articles/lang-core.html#macroexpand-1),
[`macroexpand-all`](https://willbrannon.com/arl/articles/lang-core.html#macroexpand-all),
[`suppressWarnings`](https://willbrannon.com/arl/articles/lang-core.html#suppresswarnings),
[`suppressMessages`](https://willbrannon.com/arl/articles/lang-core.html#suppressmessages),
[`with`](https://willbrannon.com/arl/articles/lang-core.html#with),
[`within`](https://willbrannon.com/arl/articles/lang-core.html#within),
[`subset`](https://willbrannon.com/arl/articles/lang-core.html#subset),
[`transform`](https://willbrannon.com/arl/articles/lang-core.html#transform),
[`substitute`](https://willbrannon.com/arl/articles/lang-core.html#substitute),
[`assert`](https://willbrannon.com/arl/articles/lang-core.html#assert),
[`assert-equal`](https://willbrannon.com/arl/articles/lang-core.html#assert-equal),
[`assert-true`](https://willbrannon.com/arl/articles/lang-core.html#assert-true),
[`assert-false`](https://willbrannon.com/arl/articles/lang-core.html#assert-false),
[`assert-eq`](https://willbrannon.com/arl/articles/lang-core.html#assert-eq),
[`assert-error`](https://willbrannon.com/arl/articles/lang-core.html#assert-error),
[`assert-no-error`](https://willbrannon.com/arl/articles/lang-core.html#assert-no-error),
[`+`](https://willbrannon.com/arl/articles/lang-core.html#plus),
[`*`](https://willbrannon.com/arl/articles/lang-core.html#star),
[`-`](https://willbrannon.com/arl/articles/lang-core.html#minus),
[`/`](https://willbrannon.com/arl/articles/lang-core.html#div),
[`<`](https://willbrannon.com/arl/articles/lang-core.html#lt),
[`<=`](https://willbrannon.com/arl/articles/lang-core.html#lte),
[`>`](https://willbrannon.com/arl/articles/lang-core.html#gt),
[`>=`](https://willbrannon.com/arl/articles/lang-core.html#gte),
[`=`](https://willbrannon.com/arl/articles/lang-core.html#num-eq),
[`==`](https://willbrannon.com/arl/articles/lang-core.html#num-eq-eq),
[`!=`](https://willbrannon.com/arl/articles/lang-core.html#bang-eq),
[`not`](https://willbrannon.com/arl/articles/lang-core.html#not),
[`eval`](https://willbrannon.com/arl/articles/lang-core.html#eval),
[`read`](https://willbrannon.com/arl/articles/lang-core.html#read),
[`write`](https://willbrannon.com/arl/articles/lang-core.html#write),
[`help`](https://willbrannon.com/arl/articles/lang-core.html#help),
[`load`](https://willbrannon.com/arl/articles/lang-core.html#load),
[`capture`](https://willbrannon.com/arl/articles/lang-core.html#capture),
[`gensym`](https://willbrannon.com/arl/articles/lang-core.html#gensym),
[`macro?`](https://willbrannon.com/arl/articles/lang-core.html#macro-p),
[`macroexpand`](https://willbrannon.com/arl/articles/lang-core.html#macroexpand),
[`promise?`](https://willbrannon.com/arl/articles/lang-core.html#promise-p),
[`force`](https://willbrannon.com/arl/articles/lang-core.html#force),
[`promise-expr`](https://willbrannon.com/arl/articles/lang-core.html#promise-expr),
[`r-eval`](https://willbrannon.com/arl/articles/lang-core.html#r-eval),
[`toplevel-env`](https://willbrannon.com/arl/articles/lang-core.html#toplevel-env),
[`builtins-env`](https://willbrannon.com/arl/articles/lang-core.html#builtins-env),
[`current-env`](https://willbrannon.com/arl/articles/lang-core.html#current-env),
[`doc!`](https://willbrannon.com/arl/articles/lang-core.html#doc-bang),
[`doc`](https://willbrannon.com/arl/articles/lang-core.html#doc),
[`module-ref`](https://willbrannon.com/arl/articles/lang-core.html#module-ref),
[`module?`](https://willbrannon.com/arl/articles/lang-core.html#module-p),
[`namespace?`](https://willbrannon.com/arl/articles/lang-core.html#namespace-p),
[`module-exports`](https://willbrannon.com/arl/articles/lang-core.html#module-exports),
[`module-name`](https://willbrannon.com/arl/articles/lang-core.html#module-name)

Modules:
[`core.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/core.arl),
[`r-interop.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/r-interop.arl),
[`assert.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/assert.arl),
and builtins

## [Types, Equality, and Conversions](https://willbrannon.com/arl/articles/lang-types.md)

Type predicates, numeric type hierarchy, structural and identity
equality, S3-style dispatch, and type conversions.

[`list?`](https://willbrannon.com/arl/articles/lang-types.html#list-p),
[`list-or-pair?`](https://willbrannon.com/arl/articles/lang-types.html#list-or-pair-p),
[`null?`](https://willbrannon.com/arl/articles/lang-types.html#null-p),
[`nil?`](https://willbrannon.com/arl/articles/lang-types.html#nil-p),
[`symbol?`](https://willbrannon.com/arl/articles/lang-types.html#symbol-p),
[`keyword?`](https://willbrannon.com/arl/articles/lang-types.html#keyword-p),
[`number?`](https://willbrannon.com/arl/articles/lang-types.html#number-p),
[`string?`](https://willbrannon.com/arl/articles/lang-types.html#string-p),
[`vector?`](https://willbrannon.com/arl/articles/lang-types.html#vector-p),
[`true?`](https://willbrannon.com/arl/articles/lang-types.html#true-p),
[`false?`](https://willbrannon.com/arl/articles/lang-types.html#false-p),
[`boolean?`](https://willbrannon.com/arl/articles/lang-types.html#boolean-p),
[`fn?`](https://willbrannon.com/arl/articles/lang-types.html#fn-p),
[`callable?`](https://willbrannon.com/arl/articles/lang-types.html#callable-p),
[`procedure?`](https://willbrannon.com/arl/articles/lang-types.html#procedure-p),
[`environment?`](https://willbrannon.com/arl/articles/lang-types.html#environment-p),
[`is-refclass?`](https://willbrannon.com/arl/articles/lang-types.html#is-refclass-p),
[`atom?`](https://willbrannon.com/arl/articles/lang-types.html#atom-p),
[`empty?`](https://willbrannon.com/arl/articles/lang-types.html#empty-p),
[`type-of`](https://willbrannon.com/arl/articles/lang-types.html#type-of),
[`real?`](https://willbrannon.com/arl/articles/lang-types.html#real-p),
[`complex?`](https://willbrannon.com/arl/articles/lang-types.html#complex-p),
[`rational?`](https://willbrannon.com/arl/articles/lang-types.html#rational-p),
[`exact?`](https://willbrannon.com/arl/articles/lang-types.html#exact-p),
[`inexact?`](https://willbrannon.com/arl/articles/lang-types.html#inexact-p),
[`integer?`](https://willbrannon.com/arl/articles/lang-types.html#integer-p),
[`natural?`](https://willbrannon.com/arl/articles/lang-types.html#natural-p),
[`finite?`](https://willbrannon.com/arl/articles/lang-types.html#finite-p),
[`infinite?`](https://willbrannon.com/arl/articles/lang-types.html#infinite-p),
[`nan?`](https://willbrannon.com/arl/articles/lang-types.html#nan-p),
[`even?`](https://willbrannon.com/arl/articles/lang-types.html#even-p),
[`odd?`](https://willbrannon.com/arl/articles/lang-types.html#odd-p),
[`zero?`](https://willbrannon.com/arl/articles/lang-types.html#zero-p),
[`positive?`](https://willbrannon.com/arl/articles/lang-types.html#positive-p),
[`negative?`](https://willbrannon.com/arl/articles/lang-types.html#negative-p),
[`non-negative?`](https://willbrannon.com/arl/articles/lang-types.html#non-negative-p),
[`non-positive?`](https://willbrannon.com/arl/articles/lang-types.html#non-positive-p),
[`identical?`](https://willbrannon.com/arl/articles/lang-types.html#identical-p),
[`eq?`](https://willbrannon.com/arl/articles/lang-types.html#eq-p),
[`eqv?`](https://willbrannon.com/arl/articles/lang-types.html#eqv-p),
[`equal?`](https://willbrannon.com/arl/articles/lang-types.html#equal-p),
[`equal?.default`](https://willbrannon.com/arl/articles/lang-types.html#equal-p-default),
[`equal?.environment`](https://willbrannon.com/arl/articles/lang-types.html#equal-p-environment),
[`equal?.list`](https://willbrannon.com/arl/articles/lang-types.html#equal-p-list),
[`env-equal?`](https://willbrannon.com/arl/articles/lang-types.html#env-equal-p),
[`list-equal?`](https://willbrannon.com/arl/articles/lang-types.html#list-equal-p),
[`s3-type`](https://willbrannon.com/arl/articles/lang-types.html#s3-type),
[`check-s3-type-match`](https://willbrannon.com/arl/articles/lang-types.html#check-s3-type-match),
[`use-method`](https://willbrannon.com/arl/articles/lang-types.html#use-method),
[`set-method!`](https://willbrannon.com/arl/articles/lang-types.html#set-method-bang),
[`symbol->string`](https://willbrannon.com/arl/articles/lang-types.html#symbol-to-string),
[`string->symbol`](https://willbrannon.com/arl/articles/lang-types.html#string-to-symbol),
[`->symbol`](https://willbrannon.com/arl/articles/lang-types.html#to-symbol),
[`->number`](https://willbrannon.com/arl/articles/lang-types.html#to-number),
[`->list`](https://willbrannon.com/arl/articles/lang-types.html#to-list),
[`->vector`](https://willbrannon.com/arl/articles/lang-types.html#to-vector),
[`->integer`](https://willbrannon.com/arl/articles/lang-types.html#to-integer),
[`->double`](https://willbrannon.com/arl/articles/lang-types.html#to-double),
[`->complex`](https://willbrannon.com/arl/articles/lang-types.html#to-complex),
[`exact->inexact`](https://willbrannon.com/arl/articles/lang-types.html#exact-to-inexact),
[`inexact->exact`](https://willbrannon.com/arl/articles/lang-types.html#inexact-to-exact),
[`pair?`](https://willbrannon.com/arl/articles/lang-types.html#pair-p)

Modules:
[`types.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/types.arl),
[`equality.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/equality.arl),
[`conversions.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/conversions.arl),
and builtins

## [Control Flow and Macros](https://willbrannon.com/arl/articles/lang-control.md)

Conditionals, binding forms, looping, threading macros, continuations,
and error handling.

[`when`](https://willbrannon.com/arl/articles/lang-control.html#when),
[`unless`](https://willbrannon.com/arl/articles/lang-control.html#unless),
[`cond`](https://willbrannon.com/arl/articles/lang-control.html#cond),
[`case`](https://willbrannon.com/arl/articles/lang-control.html#case),
[`try`](https://willbrannon.com/arl/articles/lang-control.html#try),
[`try-catch`](https://willbrannon.com/arl/articles/lang-control.html#try-catch),
[`call-cc`](https://willbrannon.com/arl/articles/lang-control.html#call-cc),
[`call-with-current-continuation`](https://willbrannon.com/arl/articles/lang-control.html#call-with-current-continuation),
[`pattern-symbols`](https://willbrannon.com/arl/articles/lang-control.html#pattern-symbols),
[`destructuring-bind`](https://willbrannon.com/arl/articles/lang-control.html#destructuring-bind),
[`let`](https://willbrannon.com/arl/articles/lang-control.html#let),
[`let*`](https://willbrannon.com/arl/articles/lang-control.html#let-star),
[`letrec`](https://willbrannon.com/arl/articles/lang-control.html#letrec),
[`when-let`](https://willbrannon.com/arl/articles/lang-control.html#when-let),
[`if-let`](https://willbrannon.com/arl/articles/lang-control.html#if-let),
[`until`](https://willbrannon.com/arl/articles/lang-control.html#until),
[`do-list`](https://willbrannon.com/arl/articles/lang-control.html#do-list),
[`loop`](https://willbrannon.com/arl/articles/lang-control.html#loop),
[`recur`](https://willbrannon.com/arl/articles/lang-control.html#recur),
[`->`](https://willbrannon.com/arl/articles/lang-control.html#thread-first),
[`->>`](https://willbrannon.com/arl/articles/lang-control.html#thread-last),
[`as->`](https://willbrannon.com/arl/articles/lang-control.html#as-to),
[`some->`](https://willbrannon.com/arl/articles/lang-control.html#some-to),
[`some->>`](https://willbrannon.com/arl/articles/lang-control.html#some-to-gt),
[`cond->`](https://willbrannon.com/arl/articles/lang-control.html#cond-to),
[`cond->>`](https://willbrannon.com/arl/articles/lang-control.html#cond-to-gt)

Modules:
[`control.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/control.arl),
[`binding.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/binding.arl),
[`looping.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/looping.arl),
[`threading.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/threading.arl)

## [Lists and Sequences](https://willbrannon.com/arl/articles/lang-list-seq.md)

List construction, accessors, association lists, sequence helpers, and
sorting.

[`call`](https://willbrannon.com/arl/articles/lang-list-seq.html#call),
[`caar`](https://willbrannon.com/arl/articles/lang-list-seq.html#caar),
[`cadr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cadr),
[`cdar`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdar),
[`cddr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cddr),
[`caaar`](https://willbrannon.com/arl/articles/lang-list-seq.html#caaar),
[`caadr`](https://willbrannon.com/arl/articles/lang-list-seq.html#caadr),
[`cadar`](https://willbrannon.com/arl/articles/lang-list-seq.html#cadar),
[`caddr`](https://willbrannon.com/arl/articles/lang-list-seq.html#caddr),
[`cdaar`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdaar),
[`cdadr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdadr),
[`cddar`](https://willbrannon.com/arl/articles/lang-list-seq.html#cddar),
[`cdddr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdddr),
[`cadddr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cadddr),
[`cddddr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cddddr),
[`list*`](https://willbrannon.com/arl/articles/lang-list-seq.html#list-star),
[`append`](https://willbrannon.com/arl/articles/lang-list-seq.html#append),
[`reverse`](https://willbrannon.com/arl/articles/lang-list-seq.html#reverse),
[`first`](https://willbrannon.com/arl/articles/lang-list-seq.html#first),
[`second`](https://willbrannon.com/arl/articles/lang-list-seq.html#second),
[`third`](https://willbrannon.com/arl/articles/lang-list-seq.html#third),
[`fourth`](https://willbrannon.com/arl/articles/lang-list-seq.html#fourth),
[`rest`](https://willbrannon.com/arl/articles/lang-list-seq.html#rest),
[`last`](https://willbrannon.com/arl/articles/lang-list-seq.html#last),
[`nth`](https://willbrannon.com/arl/articles/lang-list-seq.html#nth),
[`assoc`](https://willbrannon.com/arl/articles/lang-list-seq.html#assoc),
[`assoc-by-equal?`](https://willbrannon.com/arl/articles/lang-list-seq.html#assoc-by-equal-p),
[`assoc-by-identical?`](https://willbrannon.com/arl/articles/lang-list-seq.html#assoc-by-identical-p),
[`assoc-by-==`](https://willbrannon.com/arl/articles/lang-list-seq.html#assoc-by-eq-eq),
[`assq`](https://willbrannon.com/arl/articles/lang-list-seq.html#assq),
[`assv`](https://willbrannon.com/arl/articles/lang-list-seq.html#assv),
[`rassoc`](https://willbrannon.com/arl/articles/lang-list-seq.html#rassoc),
[`rassoc-by-equal?`](https://willbrannon.com/arl/articles/lang-list-seq.html#rassoc-by-equal-p),
[`range`](https://willbrannon.com/arl/articles/lang-list-seq.html#range),
[`iota`](https://willbrannon.com/arl/articles/lang-list-seq.html#iota),
[`make-list`](https://willbrannon.com/arl/articles/lang-list-seq.html#make-list),
[`list-ref`](https://willbrannon.com/arl/articles/lang-list-seq.html#list-ref),
[`list-tail`](https://willbrannon.com/arl/articles/lang-list-seq.html#list-tail),
[`take`](https://willbrannon.com/arl/articles/lang-list-seq.html#take),
[`drop`](https://willbrannon.com/arl/articles/lang-list-seq.html#drop),
[`take-while`](https://willbrannon.com/arl/articles/lang-list-seq.html#take-while),
[`drop-while`](https://willbrannon.com/arl/articles/lang-list-seq.html#drop-while),
[`partition`](https://willbrannon.com/arl/articles/lang-list-seq.html#partition),
[`flatten`](https://willbrannon.com/arl/articles/lang-list-seq.html#flatten),
[`repeatedly`](https://willbrannon.com/arl/articles/lang-list-seq.html#repeatedly),
[`repeat`](https://willbrannon.com/arl/articles/lang-list-seq.html#repeat),
[`zip`](https://willbrannon.com/arl/articles/lang-list-seq.html#zip),
[`member`](https://willbrannon.com/arl/articles/lang-list-seq.html#member),
[`contains?`](https://willbrannon.com/arl/articles/lang-list-seq.html#contains-p),
[`length=`](https://willbrannon.com/arl/articles/lang-list-seq.html#length-eq),
[`length>`](https://willbrannon.com/arl/articles/lang-list-seq.html#length-gt),
[`length<`](https://willbrannon.com/arl/articles/lang-list-seq.html#length-lt),
[`find`](https://willbrannon.com/arl/articles/lang-list-seq.html#find),
[`distinct`](https://willbrannon.com/arl/articles/lang-list-seq.html#distinct),
[`split-at`](https://willbrannon.com/arl/articles/lang-list-seq.html#split-at),
[`split-with`](https://willbrannon.com/arl/articles/lang-list-seq.html#split-with),
[`interpose`](https://willbrannon.com/arl/articles/lang-list-seq.html#interpose),
[`partition-by`](https://willbrannon.com/arl/articles/lang-list-seq.html#partition-by),
[`list-sort`](https://willbrannon.com/arl/articles/lang-list-seq.html#list-sort),
[`sort-by`](https://willbrannon.com/arl/articles/lang-list-seq.html#sort-by),
[`merge-sorted`](https://willbrannon.com/arl/articles/lang-list-seq.html#merge-sorted),
[`stable-sort`](https://willbrannon.com/arl/articles/lang-list-seq.html#stable-sort),
[`car`](https://willbrannon.com/arl/articles/lang-list-seq.html#car),
[`cdr`](https://willbrannon.com/arl/articles/lang-list-seq.html#cdr),
[`cons`](https://willbrannon.com/arl/articles/lang-list-seq.html#cons)

Modules:
[`list.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/list.arl),
[`sequences.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/sequences.arl),
[`sort.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/sort.arl),
and builtins

## [Strings, Display, and I/O](https://willbrannon.com/arl/articles/lang-strings-io.md)

String manipulation, file and console I/O, environment access, and
display formatting.

[`string-join`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-join),
[`string-split`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-split),
[`trim`](https://willbrannon.com/arl/articles/lang-strings-io.html#trim),
[`string-format`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-format),
[`string-contains?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-contains-p),
[`string-match?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-match-p),
[`string-find`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-find),
[`string-replace`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-replace),
[`string-replace-all`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-replace-all),
[`string-append`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-append),
[`->string`](https://willbrannon.com/arl/articles/lang-strings-io.html#to-string),
[`char-at`](https://willbrannon.com/arl/articles/lang-strings-io.html#char-at),
[`string-ref`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-ref),
[`string-slice`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-slice),
[`string-length`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-length),
[`string-upcase`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-upcase),
[`string-downcase`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-downcase),
[`string-titlecase`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-titlecase),
[`string<?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-lt-p),
[`string>?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-gt-p),
[`string=?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-eq-p),
[`string<=?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-lte-p),
[`string>=?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-gte-p),
[`string->list`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-to-list),
[`list->string`](https://willbrannon.com/arl/articles/lang-strings-io.html#list-to-string),
[`number->string`](https://willbrannon.com/arl/articles/lang-strings-io.html#number-to-string),
[`string->number`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-to-number),
[`string-prefix?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-prefix-p),
[`string-suffix?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-suffix-p),
[`string-empty?`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-empty-p),
[`string-repeat`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-repeat),
[`read-line`](https://willbrannon.com/arl/articles/lang-strings-io.html#read-line),
[`read-file`](https://willbrannon.com/arl/articles/lang-strings-io.html#read-file),
[`read-lines`](https://willbrannon.com/arl/articles/lang-strings-io.html#read-lines),
[`write-file`](https://willbrannon.com/arl/articles/lang-strings-io.html#write-file),
[`write-lines`](https://willbrannon.com/arl/articles/lang-strings-io.html#write-lines),
[`append-file`](https://willbrannon.com/arl/articles/lang-strings-io.html#append-file),
[`file-exists?`](https://willbrannon.com/arl/articles/lang-strings-io.html#file-exists-p),
[`newline`](https://willbrannon.com/arl/articles/lang-strings-io.html#newline),
[`read-from-string`](https://willbrannon.com/arl/articles/lang-strings-io.html#read-from-string),
[`write-string`](https://willbrannon.com/arl/articles/lang-strings-io.html#write-string),
[`file-size`](https://willbrannon.com/arl/articles/lang-strings-io.html#file-size),
[`file-modified-time`](https://willbrannon.com/arl/articles/lang-strings-io.html#file-modified-time),
[`file-delete`](https://willbrannon.com/arl/articles/lang-strings-io.html#file-delete),
[`directory-exists?`](https://willbrannon.com/arl/articles/lang-strings-io.html#directory-exists-p),
[`directory-list`](https://willbrannon.com/arl/articles/lang-strings-io.html#directory-list),
[`directory-delete`](https://willbrannon.com/arl/articles/lang-strings-io.html#directory-delete),
[`getenv`](https://willbrannon.com/arl/articles/lang-strings-io.html#getenv),
[`setenv`](https://willbrannon.com/arl/articles/lang-strings-io.html#setenv),
[`system-output`](https://willbrannon.com/arl/articles/lang-strings-io.html#system-output),
[`exit`](https://willbrannon.com/arl/articles/lang-strings-io.html#exit),
[`format-value`](https://willbrannon.com/arl/articles/lang-strings-io.html#format-value),
[`display`](https://willbrannon.com/arl/articles/lang-strings-io.html#display),
[`println`](https://willbrannon.com/arl/articles/lang-strings-io.html#println),
[`string-concat`](https://willbrannon.com/arl/articles/lang-strings-io.html#string-concat),
[`trace`](https://willbrannon.com/arl/articles/lang-strings-io.html#trace)

Modules:
[`strings.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/strings.arl),
[`io.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/io.arl),
[`display.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/display.arl)

## [Collections and Data Structures](https://willbrannon.com/arl/articles/lang-collections.md)

Dictionaries, sets, and struct definitions for structured data.

[`dict`](https://willbrannon.com/arl/articles/lang-collections.html#dict),
[`hash`](https://willbrannon.com/arl/articles/lang-collections.html#hash),
[`dict?`](https://willbrannon.com/arl/articles/lang-collections.html#dict-p),
[`dict-get`](https://willbrannon.com/arl/articles/lang-collections.html#dict-get),
[`dict-set`](https://willbrannon.com/arl/articles/lang-collections.html#dict-set),
[`dict-remove`](https://willbrannon.com/arl/articles/lang-collections.html#dict-remove),
[`dict-keys`](https://willbrannon.com/arl/articles/lang-collections.html#dict-keys),
[`dict-values`](https://willbrannon.com/arl/articles/lang-collections.html#dict-values),
[`dict-has?`](https://willbrannon.com/arl/articles/lang-collections.html#dict-has-p),
[`dict-merge`](https://willbrannon.com/arl/articles/lang-collections.html#dict-merge),
[`dict-update`](https://willbrannon.com/arl/articles/lang-collections.html#dict-update),
[`dict-map`](https://willbrannon.com/arl/articles/lang-collections.html#dict-map),
[`dict-filter`](https://willbrannon.com/arl/articles/lang-collections.html#dict-filter),
[`dict-for-each`](https://willbrannon.com/arl/articles/lang-collections.html#dict-for-each),
[`dict->alist`](https://willbrannon.com/arl/articles/lang-collections.html#dict-to-alist),
[`alist->dict`](https://willbrannon.com/arl/articles/lang-collections.html#alist-to-dict),
[`set`](https://willbrannon.com/arl/articles/lang-collections.html#set),
[`set?`](https://willbrannon.com/arl/articles/lang-collections.html#set-p),
[`set-add`](https://willbrannon.com/arl/articles/lang-collections.html#set-add),
[`set-remove`](https://willbrannon.com/arl/articles/lang-collections.html#set-remove),
[`set-contains?`](https://willbrannon.com/arl/articles/lang-collections.html#set-contains-p),
[`set-union`](https://willbrannon.com/arl/articles/lang-collections.html#set-union),
[`set-intersection`](https://willbrannon.com/arl/articles/lang-collections.html#set-intersection),
[`set-difference`](https://willbrannon.com/arl/articles/lang-collections.html#set-difference),
[`set->list`](https://willbrannon.com/arl/articles/lang-collections.html#set-to-list),
[`list->set`](https://willbrannon.com/arl/articles/lang-collections.html#list-to-set),
[`set-size`](https://willbrannon.com/arl/articles/lang-collections.html#set-size),
[`set-map`](https://willbrannon.com/arl/articles/lang-collections.html#set-map),
[`set-filter`](https://willbrannon.com/arl/articles/lang-collections.html#set-filter),
[`defstruct`](https://willbrannon.com/arl/articles/lang-collections.html#defstruct)

Modules:
[`dict.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/dict.arl),
[`set.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/set.arl),
[`struct.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/struct.arl)

## [Higher-Order Functions](https://willbrannon.com/arl/articles/lang-functional.md)

Mapping, filtering, folding, function composition, and logical
combinators.

[`map`](https://willbrannon.com/arl/articles/lang-functional.html#map),
[`mapcat`](https://willbrannon.com/arl/articles/lang-functional.html#mapcat),
[`filter`](https://willbrannon.com/arl/articles/lang-functional.html#filter),
[`remove`](https://willbrannon.com/arl/articles/lang-functional.html#remove),
[`reduce`](https://willbrannon.com/arl/articles/lang-functional.html#reduce),
[`foldl`](https://willbrannon.com/arl/articles/lang-functional.html#foldl),
[`foldr`](https://willbrannon.com/arl/articles/lang-functional.html#foldr),
[`every?`](https://willbrannon.com/arl/articles/lang-functional.html#every-p),
[`any?`](https://willbrannon.com/arl/articles/lang-functional.html#any-p),
[`complement`](https://willbrannon.com/arl/articles/lang-functional.html#complement),
[`compose`](https://willbrannon.com/arl/articles/lang-functional.html#compose),
[`partial`](https://willbrannon.com/arl/articles/lang-functional.html#partial),
[`curry`](https://willbrannon.com/arl/articles/lang-functional.html#curry),
[`juxt`](https://willbrannon.com/arl/articles/lang-functional.html#juxt),
[`constantly`](https://willbrannon.com/arl/articles/lang-functional.html#constantly),
[`iterate`](https://willbrannon.com/arl/articles/lang-functional.html#iterate),
[`iterate-until`](https://willbrannon.com/arl/articles/lang-functional.html#iterate-until),
[`memoize`](https://willbrannon.com/arl/articles/lang-functional.html#memoize),
[`for-each`](https://willbrannon.com/arl/articles/lang-functional.html#for-each),
[`count`](https://willbrannon.com/arl/articles/lang-functional.html#count),
[`map-indexed`](https://willbrannon.com/arl/articles/lang-functional.html#map-indexed),
[`group-by`](https://willbrannon.com/arl/articles/lang-functional.html#group-by),
[`frequencies`](https://willbrannon.com/arl/articles/lang-functional.html#frequencies),
[`not`](https://willbrannon.com/arl/articles/lang-core.html#not),
[`xor`](https://willbrannon.com/arl/articles/lang-functional.html#xor)

Modules:
[`functional.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/functional.arl),
[`logic.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/logic.arl)

## [Math and Numeric Functions](https://willbrannon.com/arl/articles/lang-math.md)

Arithmetic, comparison, rounding, trigonometry, number theory, and
complex number utilities.

[`%`](https://willbrannon.com/arl/articles/lang-math.html#percent),
[`inc`](https://willbrannon.com/arl/articles/lang-math.html#inc),
[`dec`](https://willbrannon.com/arl/articles/lang-math.html#dec),
[`clamp`](https://willbrannon.com/arl/articles/lang-math.html#clamp),
[`within?`](https://willbrannon.com/arl/articles/lang-math.html#within-p),
[`signum`](https://willbrannon.com/arl/articles/lang-math.html#signum),
[`expt`](https://willbrannon.com/arl/articles/lang-math.html#expt),
[`quotient`](https://willbrannon.com/arl/articles/lang-math.html#quotient),
[`remainder`](https://willbrannon.com/arl/articles/lang-math.html#remainder),
[`modulo`](https://willbrannon.com/arl/articles/lang-math.html#modulo),
[`gcd`](https://willbrannon.com/arl/articles/lang-math.html#gcd),
[`lcm`](https://willbrannon.com/arl/articles/lang-math.html#lcm),
[`make-rectangular`](https://willbrannon.com/arl/articles/lang-math.html#make-rectangular),
[`make-polar`](https://willbrannon.com/arl/articles/lang-math.html#make-polar),
[`real-part`](https://willbrannon.com/arl/articles/lang-math.html#real-part),
[`imag-part`](https://willbrannon.com/arl/articles/lang-math.html#imag-part),
[`magnitude`](https://willbrannon.com/arl/articles/lang-math.html#magnitude),
[`angle`](https://willbrannon.com/arl/articles/lang-math.html#angle)

Modules:
[`math.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/math.arl)

## Source files

Built-in functions are defined in
[`R/engine.R`](https://github.com/wwbrannon/arl/blob/main/R/engine.R).
The Arl stdlib modules are organized by topic in
[`inst/arl/`](https://github.com/wwbrannon/arl/tree/main/inst/arl) (each
file defines a module). The engine loads these modules in dependency
order when initializing.

- [`core.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/core.arl)
- [`list.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/list.arl)
- [`types.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/types.arl)
  (type predicates, numeric type hierarchy)
- [`logic.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/logic.arl)
  (logical operations)
- [`conversions.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/conversions.arl)
  (type conversions)
- [`equality.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/equality.arl)
  (equality and S3 dispatch)
- [`control.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/control.arl)
- [`functional.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/functional.arl)
- [`sequences.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/sequences.arl)
- [`sort.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/sort.arl)
- [`struct.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/struct.arl)
- [`threading.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/threading.arl)
- [`binding.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/binding.arl)
- [`looping.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/looping.arl)
- [`dict.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/dict.arl)
- [`math.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/math.arl)
- [`set.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/set.arl)
- [`strings.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/strings.arl)
- [`display.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/display.arl)
- [`io.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/io.arl)
- [`assert.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/assert.arl)
- [`r-interop.arl`](https://github.com/wwbrannon/arl/blob/main/inst/arl/r-interop.arl)

If you’re looking for implementation details, these files are the source
of truth for the stdlib definitions.
