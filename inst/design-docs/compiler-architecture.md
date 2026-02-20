# Compiler Architecture

## Overview

Arl compiles directly to **R language objects** (expressions built with
`as.call()`, `as.symbol()`, `quote()`) rather than to bytecode or a custom VM.
Compiled Arl is first-class R code evaluated via `base::eval()` in the
appropriate environment.

The pipeline is: Arl source → tokens → AST → macro expansion → R language
object → `eval()`. Every stage is implemented in pure R.

## Why R Language Objects?

Compiling to R's own expression representation provides several advantages
over a bytecode VM:

1. **Seamless R interop.** Compiled code runs in R's execution model, so
   calling R functions, accessing R data structures, and using R's error
   handling all work natively with no FFI layer.
2. **Debuggability.** Compiled expressions can be inspected, deparsed, and
   printed before evaluation. Stack traces reference recognizable R constructs.
3. **Caching.** Compiled expressions serialize naturally as `.rds` files via
   the module cache (see `cache-invalidation.md`).
4. **Source attribution.** R's built-in attribute system carries source
   metadata through compilation and execution for error reporting.

## Core Compilation Functions

- **`compile(expr)`** (`compiler.R`): Main entry point. Returns an R language
  object or NULL on error.
- **`compile_seq(exprs)`**: Compiles a list of expressions into a `{}`
  block: `as.call(c(list(quote(`{`)), compiled))`.
- **`compile_impl(expr)`**: Core recursive compiler. Dispatches on special
  forms via a `switch(op_name, ...)` statement.

When `strict = FALSE` (default), compilation errors return NULL and store the
error in `self$last_error`. When `strict = TRUE`, errors are raised
immediately. Tests and internal contexts use strict mode.

## Special Form Dispatch

The compiler recognizes ~15 special forms, each with a dedicated handler:

| Special Form | Handler | Behavior |
|---|---|---|
| `quote` | `compile_quote` | Returns `(quote x)` |
| `quasiquote` | `compile_quasiquote` | Template expansion with unquote/splicing |
| `if` | `compile_if` | Conditional; dead code elimination if test is constant |
| `begin` | `compile_begin` | Block of sequential expressions |
| `define` | `compile_define` | Binding via `.__assign_pattern()` (supports destructuring) |
| `set!` | `compile_set` | Mutation; walks parent chain |
| `lambda` | `compile_lambda` | Function creation; TCO detection happens here |
| `and` / `or` | `compile_short_circuit` | Short-circuit boolean evaluation |
| `while` | `compile_while` | Loop compilation |
| `delay` | `compile_delay` | Lazy promise creation |
| `defmacro` | `compile_defmacro` | Macro definition |
| `import` | `compile_import` | Module import (top-level only) |
| `module` | `compile_module` | Module definition |

Anything that is not a special form goes through `compile_application()`,
which handles ordinary function calls and is also where strength reduction
and constant folding optimizations apply.

## Self-Tail-Call Optimization (Self-TCO)

### Motivation

R does not optimize tail calls. A recursive function that works in Scheme or
Clojure overflows R's call stack after a few thousand iterations. The compiler
fixes this for the most common case: **self-recursion**.

### How It Works

When the compiler sees `(define name (lambda ...))` or
`(set! name (lambda ...))`, it checks whether the lambda body contains calls
to `name` in tail position. If so, the entire function is rewritten as a
`while(TRUE)` loop where each self-call becomes parameter reassignment
followed by `next`.

**Step 1 — Detection.** `has_self_tail_calls()` walks the AST looking for
calls to the function's own name in tail position. It recognizes tail position
through `if` (both branches), `begin` (last expression), and any macros that
expand to these (like `cond`, `let`, `let*`, `letrec`). The walk does not
descend into nested `lambda`, `quote`, or `quasiquote` forms, since those
create new contexts.

**Step 2 — Transformation.** The last expression in the body is compiled in
"tail position mode" via `compile_tail_position()`. When a self-call is found:

- All new argument values are computed first.
- If only one parameter changes, it is assigned directly.
- If two or more change, temporaries (`.__tco_<param>`) ensure all values are
  computed before any assignment (like a simultaneous `let`).
- A `next` statement continues the loop.

**Step 3 — Wrapping.** The body is wrapped in `while(TRUE) { ... }` with
non-tail returns wrapped in `return()`.

Example:
```lisp
(define factorial
  (lambda (n acc)
    (if (< n 2)
      acc
      (factorial (- n 1) (* acc n)))))
```

Compiles (roughly) to:
```r
function(n, acc) {
  .__env <- environment()
  while (TRUE) {
    if (n < 2) {
      return(acc)
    } else {
      .__tco_n <- n - 1
      .__tco_acc <- acc * n
      n <- .__tco_n
      acc <- .__tco_acc
      next
    }
  }
}
```

### IIFE Inlining

Immediately-invoked lambdas in tail position (`((lambda (x) body) arg)`) are
inlined: the parameter assignment is extracted and the body is compiled in
tail position within the enclosing loop. This avoids creating a new closure on
each iteration.

### Disabling TCO

TCO can be disabled for debugging via `Engine$new(disable_tco = TRUE)`,
`options(arl.disable_tco = TRUE)`, or `ARL_DISABLE_TCO=1`. With TCO disabled,
self-recursive functions compile to normal recursive calls, preserving full
stack traces at the cost of stack overflow on deep recursion.

## Optimizations

### Constant Folding

`try_constant_fold()` evaluates pure function calls at compile time when all
arguments are literals.

**Whitelist:** Only deterministic, side-effect-free functions are folded:
arithmetic (`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`), comparison (`<`, `>`,
`<=`, `>=`, `==`, `!=`), logical (`&`, `|`, `!`), math (`abs`, `sqrt`, `exp`,
`log`, `sin`, `cos`, ...), and string operations (`nchar`, `toupper`,
`tolower`).

**Literal recognition:** `NULL`, `TRUE`, `FALSE`, `NA`, `NaN`, `Inf`, and
atomic vectors (numbers, strings, logicals).

Folding is disabled when coverage tracking is active (instrumented code needs
source info) or inside `delay` bodies.

### Strength Reduction

`try_strength_reduce()` replaces expensive operations with cheaper
equivalents:

- `(* x 2)` or `(* 2 x)` → `(+ x x)` (when `x` is a symbol or literal)
- `(^ x 2)` → `(* x x)` (when `x` is a symbol or literal)

The side-effect guard is important: only symbols and atomic values can safely
be duplicated. An expression like `(* (f) 2)` is not reduced because `(f)`
might have side effects that should not execute twice.

### Identity Elimination

Immediately-invoked identity lambdas `((lambda (x) x) value)` are inlined to
just `value`. Only applies to single-parameter lambdas with no defaults,
patterns, or rest parameters.

### And/Or Flattening

Nested boolean forms are flattened before compilation:
`(and (and a b) c)` → `(and a b c)`. This avoids unnecessary nesting in the
compiled output.

### Dead Code Elimination

In `compile_if`, when the test is a compile-time constant (`#t`, `#f`, `#nil`),
only the taken branch is emitted. The unused branch is still compiled (for
validation) but its result is discarded.

## The `.__env` Convention

Every compiled lambda begins with `.__env <- environment()`. This captures
the closure's R environment so that `define`, `set!`, and `import` can modify
the correct scope. The `.__` prefix is reserved; user code cannot define names
starting with `.__`.

## Nesting Depth Tracking

The compiler tracks `nesting_depth` (incremented on `lambda`, `if`, `while`,
`delay`) to enforce that `import` only appears at the top level
(`nesting_depth == 0`). This prevents confusing scoping issues from imports
inside conditionals or loops.

## Truthiness

Arl truthiness differs from R: only `#f` (FALSE), `#nil` (NULL), and `0` are
falsy. The compiler wraps `if` tests in `.__true_p()` to enforce this, unless
the test is known to return an R logical (e.g., `<`, `>`, `==`, `null?`), in
which case the wrapper is skipped for performance.

## Coverage Instrumentation

When `context$coverage_tracker` is present, the compiler injects
`.__coverage_track(file, start_line, end_line)` calls:

- **Lambda bodies:** Before each statement in the body.
- **If branches:** Wrapping each branch expression.

Coverage calls are built from source metadata attributes (`arl_src`). When
coverage is disabled, no calls are injected — zero runtime overhead.

## EvalContext: Shared State

The `EvalContext` object (defined in `runtime.R`) is shared by the Compiler,
MacroExpander, and CompiledRuntime. It provides:

| Field | Purpose |
|---|---|
| `env` | Current Arl environment (wraps R env) |
| `source_tracker` | Source location metadata for error reporting |
| `macro_expander` | Macro expansion (set by Engine after creation) |
| `compiler` | Compiler instance (set by Engine after creation) |
| `compiled_runtime` | Runtime helpers (set by Engine after creation) |
| `coverage_tracker` | Optional; enables coverage instrumentation |
| `builtins_env` | R env containing `=`, `==`, `!=`, internal helpers |
| `prelude_env` | R env containing prelude module exports |
| `loading_modules` | Stack of currently-loading modules (cycle detection) |
| `squash_imports` | TRUE during prelude loading (dumps exports directly) |

The context is created once per Engine and links all components together,
providing a single source of truth for shared state like the macro registry,
module loading stack, and environment chain.
