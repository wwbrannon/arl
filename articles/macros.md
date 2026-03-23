# Macros and Quasiquote

Macros let you transform code at compile time, before evaluation. Arl
provides `defmacro` for defining macros, quasiquote/unquote for building
output templates, automatic hygiene to prevent accidental variable
capture, `gensym` for generating unique symbols, and `capture` for
intentionally breaking hygiene (anaphoric macros).

## Overview: what kind of macros does Arl support?

Arl macros are **procedural, defmacro-style** macros: each macro is an
ordinary function that receives its arguments as unevaluated syntax and
returns new syntax. This is the same model used by Common Lisp and
Clojure, as opposed to Scheme’s pattern-based `syntax-rules`.

On top of this, Arl adds **automatic hygiene**. Bindings introduced by a
macro body are automatically renamed so they cannot shadow the caller’s
variables. This means that simple macros are hygienic *by default* – you
do not need to call `gensym` for every temporary. When you do want to
intentionally introduce a binding visible to the caller (an anaphoric
macro), you use `capture` to opt out of hygiene for specific symbols.

In summary:

- **Hygienic by default** – macro-introduced `define`, `let`, and
  `lambda` bindings are automatically renamed.
- **Unhygienic escape via `capture`** – marks a symbol so hygiene leaves
  it alone, allowing intentional capture.
- **`gensym` available** – generates unique symbols for cases where you
  need explicit control (e.g. macro-time computation that builds binding
  forms dynamically).

## Defining a macro

    arl> ;;' @description Evaluate body forms when test is truthy.
    arl> (defmacro when (test . body)
    arl>   `(if ,test (begin ,@body) #nil))

With that macro defined:

    arl> ;; expands to: (if (> 5 3) (begin (print "yes")) #nil)
    arl> ;; note that the first `[1] "yes"` is the return value,
    arl> ;; while the second `"yes"` is the print output
    arl> (when (> 5 3) (print "yes"))
    #> [1] "yes"
    #> "yes"

Macros receive their arguments *unevaluated* – the forms are passed as
syntax trees (R calls/symbols), not as values. The macro body constructs
and returns new syntax, which the compiler then compiles and evaluates
in place of the original call.

You can document macros with `;;'` annotation comments above the
definition (see [Getting Started – Documenting
functions](https://willbrannon.com/arl/articles/getting-started.html#documenting-functions)),
and use `help` to view this documentation from the REPL:

``` arl
(help "when")
```

## Macro parameters

### Fixed parameters

    arl> (defmacro double (x)
    arl>   `(* 2 ,x))

    arl> (double (+ 1 2))  ; => 6
    #> 6

### Rest parameters

Use `.` to collect remaining arguments into a list:

    arl> (defmacro unless (test . body)
    arl>   `(if ,test #nil (begin ,@body)))

    arl> (unless (= 1 2) (+ 20 22))  ; => 42
    #> 42

### Optional parameters with defaults

Parameters wrapped in a pair `(name default)` are optional:

    arl> (defmacro greet ((name "world"))
    arl>   `(string-concat "hello, " ,name))

    arl> (greet)          ; => "hello, world"
    #> "hello, world"
    arl> (greet "Alice")  ; => "hello, Alice"
    #> "hello, Alice"

You can mix required and optional parameters, and combine them with
rest:

    arl> (defmacro opt-rest ((x 1) . rest)
    arl>   `(list ,x ,@rest))

    arl> (opt-rest)         ; => (1)
    #> (1)
    arl> (opt-rest 2 3 4)   ; => (2 3 4)
    #> (2 3 4)

### Pattern destructuring in parameters

Use `(pattern ...)` to destructure a macro argument (i.e., unpack it
into multiple variables, as in Python’s `x, y = obj`):

    arl> (defmacro let-pair ((pattern (name value)))
    arl>   `(define ,name ,value))

    arl> (let-pair (x 10))
    #> 10
    arl> x  ; => 10
    #> 10

Patterns can be nested, have defaults, or be used as rest parameters:

    arl> ;; Nested pattern
    arl> (defmacro deep ((pattern (a (b c))))
    arl>   `(list ,a ,b ,c))

    arl> (deep (1 (2 3)))  ; => (1 2 3)
    #> (1 2 3)

    arl> ;; Pattern with default
    arl> (defmacro with-point ((pattern (x y) (list 0 0)))
    arl>   `(+ ,x ,y))

    arl> (with-point)        ; => 0
    #> 0
    arl> (with-point (3 4))  ; => 7
    #> 7

## Quasiquote, unquote, and splicing

Quasiquote (backtick) is the primary tool for constructing macro output.
Inside a quasiquoted template, `,expr` (unquote) evaluates `expr` and
inserts the result into the template, and `,@expr` (unquote-splicing)
evaluates `expr` (which must produce a list) and splices its elements
into the template on the same level as `,@expr`.

### Quasiquote basics

    arl> (define x 10)
    #> 10
    arl> `(+ ,x 20)      ; => (+ 10 20)
    #> (+ 10 20)

Concisely, the difference between `,` and `,@` is that `,` always
replaces `,expr` with one element, but `,@`, if the list to be spliced
has length greater than 1, can replace `,@expr` with more than one
element.

### Splicing lists

    arl> (define xs (list 2 3))
    #> (2 3)
    arl> `(+ 1 ,@xs 4)   ; => (+ 1 2 3 4)
    #> (+ 1 2 3 4)

### Nested quasiquote

When quasiquotes are nested, each level of backtick adds one to the
depth, and each comma subtracts one. Only depth-zero unquotes are
evaluated:

    arl> `(a `(b ,,x))   ; => (a (quasiquote (b (unquote 10))))
    #> (a (quasiquote (b (unquote 10))))

This is useful for writing macros that generate other macros.

## Hygiene

Arl macros are **hygienic by default**. When a macro introduces bindings
(via `define` or other macros that expand into a `define`), those
bindings are automatically renamed so they cannot collide with names at
the call site.

### Automatic hygiene example

Consider a `swap` macro that uses a temporary variable:

    arl> (defmacro swap (a b)
    arl>   `(let ((temp ,a))
    arl>      (set! ,a ,b)
    arl>      (set! ,b temp)))

Without hygiene, if the caller had a variable named `temp`, the macro
would silently shadow it. In Arl, the macro-introduced `temp` is
automatically renamed to a fresh symbol, so caller bindings are safe:

    arl> (define temp 999)
    #> 999
    arl> (define x 1)
    #> 1
    arl> (define y 2)
    #> 2
    arl> (swap x y)
    arl> x     ; => 2
    #> 2
    arl> y     ; => 1
    #> 1
    arl> temp  ; => 999  (unaffected)
    #> 999

### How it works

After a macro function returns its expansion, the expander runs a
*hygienization* pass. This pass walks the expanded code and renames any
symbols that the macro introduced (i.e. not originating from the call
site) in binding positions like those of `define` or `lambda`
parameters. Symbols originating from the caller’s code are marked and
left untouched.

The quasiquote expander tags each evaluated value with its origin,
marking unquoted caller expressions in particular as having originated
from the containing scope. The hygienizer then uses these tags to decide
which symbols to rename.

## `gensym` – generating fresh symbols

`gensym` creates a unique symbol guaranteed not to collide with any
user-defined name. Use it when you need to create bindings at
macro-expansion time that must not conflict:

    arl> (gensym)          ; => G__N for some N
    #> G__15
    arl> (gensym "tmp")    ; => tmp__N for some N
    #> tmp__16

Each call returns a new symbol with a monotonically increasing counter,
incrementing further to skip over any symbols which are already defined.

### When to use `gensym`

Because Arl macros are hygienic by default, you usually do *not* need
`gensym` for simple temporaries in quasiquoted output. `gensym` is
useful when you build forms *programmatically at expansion time* and
need a consistent temporary name across all of them – for example, when
`map` generates a variable number of forms that must share a binding:

    arl> ;; bind-all: evaluate expr once, then define each name to the result.
    arl> (defmacro bind-all (expr . names)
    arl>   (define tmp (gensym "val"))
    arl>   `(begin
    arl>      (define ,tmp ,expr)
    arl>      ,@(map (lambda (n) `(define ,n ,tmp)) names)))

    arl> (bind-all (+ 1 2) a b c)
    #> 3
    arl> (list a b c)
    #> (3 3 3)

Here `map` builds one `define` per name, and every generated form must
reference the same temporary. Because these forms are constructed
programmatically rather than written directly in a quasiquote template,
automatic hygiene won’t save us: we need `gensym` to ensure the binding
is unique.

Another common pattern is loop macros:

    arl> (defmacro do-list (binding . body)
    arl>   (begin
    arl>     (define var (car binding))
    arl>     (define seq (car (cdr binding)))
    arl>     (define remaining (gensym "do_list_remaining"))
    arl>     `(begin
    arl>        (define ,remaining (_as-list ,seq))
    arl>        (while (not (null? ,remaining))
    arl>          (define ,var (car ,remaining))
    arl>          ,@body
    arl>          (set! ,remaining (cdr ,remaining))))))

    arl> (do-list (x '(1 2 3))
    arl>   (print x))
    #> [1] 1
    #> [1] 2
    #> [1] 3

## `capture` – intentional variable capture (anaphoric macros)

Sometimes you *want* a macro to introduce a binding visible to the
caller. The classic example is the **anaphoric if** (`aif`), which binds
the test result to the fixed symbol `it` so the then/else branches can
use it.

Without any special handling, hygiene would rename the macro’s `it` to a
fresh symbol, making it invisible to the caller. The `capture` builtin
overrides this: it marks a specific symbol as “intentionally
introduced”, telling the hygienizer to leave it alone.

### Signature

``` arl
(capture 'symbol expr)
```

- `symbol` – the symbol name to preserve (quoted).
- `expr` – the expression in which occurrences of `symbol` should remain
  unhygienic.

### Example: anaphoric if

    arl> (defmacro aif (test then alt)
    arl>   `(let ((it ,test))
    arl>      (if it ,(capture 'it then) ,(capture 'it alt))))

Usage:

    arl> (import display :refer (string-concat))

    arl> (aif (+ 2 3)
    arl>      (string-concat "result is " it)   ; it => 5
    arl>      "no result")  ; => "result is 5"
    #> "result is 5"

Here `capture` is applied to the `then` and `alt` expressions (which
came from the caller). It marks every occurrence of `it` inside those
expressions as “introduced” rather than “call_site”, so hygiene does not
rename them. Meanwhile, the `(let ((it ,test)) ...)` binding itself is
in the macro’s quasiquoted output, and `capture` ensures the two sides
agree on the name `it`.

### How `capture` interacts with hygiene

1.  The macro returns a quasiquoted expansion containing
    `let ((it ...)) ...`.
2.  Normally, hygiene would rename the macro’s `it` to a fresh symbol.
3.  `capture` walks `then` and `alt` and marks each `it` symbol with
    origin `"introduced"`.
4.  Because both the binding and the references now have the same
    origin, the hygienizer treats them as belonging to the same scope
    and does not rename them.

Without `capture`, the caller would not be able to refer to `it`:

``` arl
;; BROKEN -- without capture, hygiene renames 'it'
(defmacro bad-aif (test then alt)
  `(let ((it ,test))
     (if it ,then ,alt)))

(bad-aif (+ 2 3) it 0)  ; it here refers to the CALLER's 'it', not the macro's
```

## Compile-time computation

Because the macro body is ordinary Arl code executed at expansion time,
you can perform arbitrary computation before returning the template:

    arl> (defmacro const-multiply (a b)
    arl>   (let ((result (* a b)))
    arl>     `(quote ,result)))

    arl> (const-multiply 6 7)  ; => 42 (computed at macro-expansion time)
    #> 42

## Recursive and composing macros

Macros can call themselves recursively:

    arl> ;; Thread-first: recursive macro that threads a value through forms
    arl> (defmacro -> (value . forms)
    arl>   (if (null? forms)
    arl>       value
    arl>       (let ((first-form (car forms))
    arl>             (rest-forms (cdr forms)))
    arl>         (if (list-or-pair? first-form)
    arl>             `(-> (,(car first-form) ,value ,@(cdr first-form)) ,@rest-forms)
    arl>             `(-> (,first-form ,value) ,@rest-forms)))))

    arl> ;; expands to (- (* (+ 5 3) 2) 1)
    arl> (-> 5 (+ 3) (* 2) (- 1))  ; => 15
    #> 15

Macros can also expand into calls to other macros. The expander
recursively expands until no macro calls remain:

    arl> ;; when-not expands into unless, which expands into if
    arl> (defmacro when-not (test . body)
    arl>   `(unless ,test ,@body))

    arl> ;; two levels of expansion: when-not -> unless -> if
    arl> (when-not (= 1 2) (+ 20 22))  ; => 42
    #> 42

## Inspecting macro expansions

### From Arl

Use the `macroexpand` function to perform macro expansion on an
expression at execution time. `macroexpand` supports an optional `depth`
argument:

- no depth: fully and recursively expands all macro calls
- `depth = 1` (or `:depth 1`): one expansion step at the outermost call
- `depth = N`: exactly `N` expansion steps

`macroexpand-1` is a convenience alias for one-step expansion,
equivalent to `(macroexpand expr 1)` and `(macroexpand expr :depth 1)`:

    arl> (macroexpand '(when #t 1) 1) ; => (if #t (begin 1) #nil)
    #> (if TRUE (begin 1) .__nil)

    arl> (macroexpand '(when #t 1) :depth 1) ; => (if #t (begin 1) #nil)
    #> (if TRUE (begin 1) .__nil)

    arl> (macroexpand-1 '(when #t 1)) ; => (if #t (begin 1) #nil)
    #> (if TRUE (begin 1) .__nil)

For full recursive expansion, use `macroexpand` with no depth (or its
alias `macroexpand-all`):

    arl> (macroexpand '(when #t (unless #f 42)))
    #> (if TRUE (begin (if FALSE .__nil (begin 42))) .__nil)

`macro?` tests whether a symbol names a currently defined macro:

    arl> (macro? 'when)   ; => #t
    #> TRUE
    arl> (macro? 'car)    ; => #f
    #> FALSE

### From R

Use `engine$macroexpand()`, which takes the same `depth` argument, to
expand from R:

``` r
engine <- Engine$new()
engine$eval(engine$read("(defmacro when (test . body) `(if ,test (begin ,@body) #nil))")[[1]])
engine$macroexpand(engine$read("(when #t 1)")[[1]])
```

To see the full compilation pipeline (parsed, expanded, compiled R
code), use `engine$inspect_compilation(text)`. It returns a list with
`parsed`, `expanded`, `compiled`, and `compiled_deparsed`. See
[`?Engine`](https://willbrannon.com/arl/reference/Engine.md) for
details.

## Real-world examples from the standard library

As in most Lisps, many of Arl’s core forms are implemented as macros.
Here are some patterns worth studying:

### `let` – parallel bindings with `gensym`

``` arl
(defmacro let (bindings . body)
  (if (null? bindings)
    `(begin ,@body)
    (begin
      (define temps (map (lambda (b) (gensym "tmp")) bindings))
      (define patterns (map car bindings))
      (define values (map (lambda (b) (car (cdr b))) bindings))
      `((lambda ,temps
          ,@(map (lambda (pair)
                   `(define ,(car pair) ,(car (cdr pair))))
                 (zip patterns temps))
          (begin ,@body))
        ,@values))))
```

This macro generates one `gensym` per binding, then creates a `lambda`
with those fresh names as parameters. Inside the lambda body, each fresh
name is destructured into the user’s pattern. The `gensym` calls are
necessary here because the binding names are computed programmatically
rather than appearing literally in a quasiquoted template.

### `loop`/`recur` – Clojure-style iteration

The `loop` macro rewrites `recur` calls into a `while` loop with flag
variables, all using `gensym` to avoid conflicts:

    arl> (import looping :refer (loop recur))

    arl> (loop ((n 5) (acc 1))
    arl>   (if (= n 0)
    arl>     acc
    arl>     (recur (- n 1) (* acc n)))) ; => 120
    #> 120

### `try`/`catch`/`finally` – syntax sugar

The `try` macro parses its clause list at expansion time and rewrites
into calls to the lower-level `try` function (which in turn calls R’s
`tryCatch`):

    arl> (try-catch (stop "something went wrong")
    arl>   (catch e (string-concat "caught: " ($ e "message")))
    arl>   (finally (display "cleanup")))
    #> "cleanup" 
    #> "caught: something went wrong"

## Related guides

- [Getting
  Started](https://willbrannon.com/arl/articles/getting-started.md)
- [Language
  Reference](https://willbrannon.com/arl/articles/lang-reference.md)
- [Standard Library: Core, R Interop, and
  Testing](https://willbrannon.com/arl/articles/lang-core.md)
- [Examples](https://willbrannon.com/arl/articles/examples.html#macro-examples)
