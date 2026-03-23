# Troubleshooting

Common pitfalls and how to resolve them.

## Zero is falsy

Unlike many Lisps, Arl treats `0` as falsy (along with `#f`/`FALSE`,
`#nil`/`NULL`). This matches R’s convention where `0` is equivalent to
`FALSE`.

    arl> (if 0 "truthy" "falsy")   ; => "falsy"
    #> "falsy"
    arl> (if 1 "truthy" "falsy")   ; => "truthy"
    #> "truthy"
    arl> (when 0 (print "nope"))   ; => #nil

If you need to distinguish zero from false, test explicitly:

``` arl
(if (not (nil? x)) ...)   ; check for nil/NULL
(if (number? x) ...)      ; check for any number
```

## `list?` vs `base::is.list` on quoted forms

Under the hood, quoted forms are R [language/call
objects](https://cran.r-project.org/doc/manuals/r-devel/R-lang.html#Computing-on-the-language),
not R lists. Arl still treats calls as list-like for Lisp semantics, so
these can disagree:

    arl> (list? '(1 2 3))           ; => #t
    #> TRUE
    arl> (base::is.list '(1 2 3))   ; => #f
    #> FALSE

Use `list?` when writing Arl code that follows Lisp list semantics. Use
[`base::is.list`](https://rdrr.io/r/base/list.html) only when you
specifically need R’s underlying object type.

## Use `#t` / `#f`, not `T` / `F`

In R, `T` and `F` are ordinary variables (not reserved words) that
happen to be bound to `TRUE` and `FALSE` by default. In Arl, use the
literal boolean syntax instead:

    arl> ;; Correct
    arl> (if #t "yes" "no")
    #> "yes"
    arl> (define flag #f)
    #> FALSE

    arl> ;; Fragile -- T/F can be rebound
    arl> (define T 42)
    #> 42
    arl> (if T "oops" "no")   ; => "oops" (42 is truthy)
    #> "oops"

## `load` vs `import` vs `run`

These three forms load code differently:

| Form                    | Scope          | Definitions visible? | Use case                                      |
|-------------------------|----------------|----------------------|-----------------------------------------------|
| `(load "file.arl")`     | Current env    | Yes                  | Source a script into REPL                     |
| `(load "file.arl" env)` | `env`          | Yes                  | Source into a chosen environment              |
| `(import module)`       | Current scope  | Exports only         | Use a module’s public API                     |
| `(run "file.arl")`      | Isolated child | No                   | Execute a script without binding side effects |

From R:

- `engine$load_file_in_env(path)` – definitions visible (like `load`)
- `engine$load_file_in_env(path, new.env(parent = env))` – isolated
  scope (like `run`)

If you get “symbol not found” after loading a file, you may have used
`run` or an isolated child environment when you needed `load` or
`engine$load_file_in_env()`.

## Self-TCO limitations

The compiler’s self-tail-call optimization applies to
`(define name (lambda ...))` and `(set! name (lambda ...))` forms, which
means `letrec`-bound lambdas are also covered. However, some patterns
are **not** optimized:

``` arl
;; TCO works here
(define factorial
  (lambda (n acc)
    (if (< n 2) acc
      (factorial (- n 1) (* acc n)))))

;; TCO does NOT apply -- mutual recursion (each function calls the other,
;; not itself, so self-TCO cannot help)
(define is-even?
  (lambda (n)
    (if (= n 0) #t
      (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0) #f
      (is-even? (- n 1)))))

;; This will overflow the stack for large n
(is-even? 100000)
```

For mutual recursion, anonymous lambdas, or other cases where self-TCO
does not apply, use `loop`/`recur` from the `looping` module:

``` arl
(loop ((i n) (acc 1))
  (if (< i 2) acc
    (recur (- i 1) (* acc i))))
```

## Macro hygiene and `gensym`

Arl macros are **hygienic by default** – bindings introduced by a macro
(via `define`, `let`, `lambda`, etc.) are automatically renamed so they
cannot collide with names at the call site. This means simple macros
work without any extra effort:

``` arl
;; This is safe -- hygiene auto-renames 'tmp' so it won't
;; shadow a caller's variable named 'tmp'
(defmacro my-swap (a b)
  `(let ((tmp ,a))
     (set! ,a ,b)
     (set! ,b tmp)))
```

You need `gensym` when you build binding forms **programmatically at
expansion time** – for example, computing a list of bindings in a loop
before constructing the quasiquoted result. In that case, automatic
hygiene cannot track the symbols because they are created outside the
quasiquote template:

``` arl
;; gensym needed: binding name is computed, not in quasiquote template
(defmacro bind-all (pairs . body)
  (define temps (map (lambda (p) (gensym "t")) pairs))
  `((lambda ,temps ,@body)
    ,@(map (lambda (p) (car (cdr p))) pairs)))
```

See [Macros and Quasiquote –
Hygiene](https://willbrannon.com/arl/articles/macros.html#hygiene) for a
full explanation.

## Module not found errors

When `(import name)` fails, check:

1.  **Stdlib modules** are resolved from `inst/arl/` in the installed
    package. They are loaded automatically by the engine.

2.  **User modules** are resolved relative to the current working
    directory. Make sure your file is named `name.arl` and is in the
    CWD.

3.  **String imports** (`(import "path/to/file.arl")`) use path-only
    resolution – no stdlib search.

## Stack traces with TCO-optimized functions

When a TCO-optimized function errors, the stack trace shows only the
outermost call frame because the recursive calls have been compiled into
a loop. To debug:

1.  Use `engine$inspect_compilation()` to see the compiled R code.
2.  Temporarily add `(print ...)` statements inside the function body.
3.  Or disable TCO temporarily with `Engine$new(disable_tco = TRUE)` or
    `options(arl.disable_tco = TRUE)` to get full stack traces during
    debugging.

## Related guides

- [Getting
  Started](https://willbrannon.com/arl/articles/getting-started.md)
- [Compiler and
  Internals](https://willbrannon.com/arl/articles/internals.md)
- [Macros and
  Quasiquote](https://willbrannon.com/arl/articles/macros.md)
- [Modules and Imports](https://willbrannon.com/arl/articles/modules.md)
