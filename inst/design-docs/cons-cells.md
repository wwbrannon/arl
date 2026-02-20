# Design Doc: Cons Cells and Dual List Representation

## Problem

Arl needs to represent both proper lists (for R interop) and improper/dotted pairs (for Lisp semantics). R has no native cons cell type — its lists are indexed vectors, not linked pairs. This means `(cons 'a 'b)` cannot be represented as an R list, because `cdr` of an R list always returns another list, never an atom.

## Decision

Dual representation: R lists and calls for everyday proper lists, an R6 `Cons` class for dotted pairs only. The `car`/`cdr` API is unified across both representations via three-way type dispatch.

## The Cons Class

Defined in `R/cells.R`. Minimal R6 class with two public fields (`car`, `cdr`) and two traversal methods:

- `as_list()` — walks the cdr chain collecting car values into an R list (drops the final non-Cons tail)
- `parts()` — like `as_list()` but returns both the prefix (list of car values) and the tail (first non-Cons cdr)

Design choice: R6 over S3 for encapsulation and field access clarity. Cons cells are identity objects (mutable fields, reference semantics), which fits R6's model.

## When Cons Cells Are Created

**Parser** — dotted-pair literal syntax `(a . b)` produces nested `Cons$new()` calls. The parser accumulates elements before the dot into `dotted_heads`, reads one element after the dot as `dot_cdr`, then folds right:

```r
# R/parser.R — inside parse_list, dotted-pair folding
result <- dot_cdr
for (j in rev(seq_along(dotted_heads))) {
  result <- Cons$new(dotted_heads[[j]], result)
}
```

**Runtime** — `(cons x y)` in `inst/arl/list.arl` is smart about which representation to use:

```scheme
(define cons
  (lambda (item lst)
    (if (is.call lst)
      (as.call (c (list item) (as.list lst)))
      (if (is.list lst)
        (c (list item) lst)
        (.__cons item lst)))))
```

Only when `y` is neither a list nor a call does `cons` create a `Cons` cell via the `.__cons` builtin (defined in `R/engine.R`, `install_builtins`). This keeps the common case — prepending to a proper list — in native R.

## car/cdr Dispatch

Both `car` and `cdr` in `inst/arl/list.arl` use three-way type dispatch:

1. **Cons** (`pair?` is true) — field access via `(r-call "[[" (list lst "car"))` / `"cdr"`
2. **R call** (`is.call`) — element extraction with `[[` for car, sublist `[` for cdr
3. **R list** (`is.list`) — same element extraction pattern

This unifies the two representations under a single API. Both return `#nil` for empty inputs.

## Predicate Design

| Predicate | Cons | Non-empty R list/call | Empty list/NULL |
|---|---|---|---|
| `pair?` | `#t` | `#f` | `#f` |
| `list?` | `#f` | `#t` | `#t` |
| `list-or-pair?` | `#t` | `#t` | `#f` |
| `null?` | `#f` | `#f` | `#t` |
| `atom?` | `#f` | `#f` | `#t` |

`list?` excludes Cons because a Cons might be an improper pair (e.g. `(a . b)`), and `list?` should mean "safe for `length`/indexing operations." `pair?` is the Cons-specific predicate. `list-or-pair?` covers both non-empty representations.

## Compiler Handling

**Parameter lists** — When the compiler encounters a Cons in a lambda's parameter list, it decomposes it via `parts()` to extract the regular parameters (prefix) and rest parameter (tail):

```r
# R/compiler.R — inside compile_lambda, parameter normalization
} else if (r6_isinstance(args_expr, "Cons")) {
  parts <- args_expr$parts()
  c(parts$prefix, list(as.symbol(".")), list(parts$tail))
```

**As values** — Cons cells are self-evaluating atoms in the compiler (like numbers and strings). They pass through compilation unchanged.

**Quoting** — Quoted Cons round-trips through the parser/unparser. The unparser recognizes Cons and emits dotted-pair syntax.

## Builtins

Three Cons-related primitives are installed in `builtins_env` (see `install_builtins` in `R/engine.R`):

- `.__cons` — creates `Cons$new(car, cdr)`
- `.__cons-as-list` — calls `as_list()` on a Cons (or returns empty list)
- `.__cons-parts` — calls `parts()` on a Cons (or returns trivial prefix/tail)
- `pair?` — tests `r6_isinstance(x, "Cons")`

## Tradeoffs

**Pros**:
- R interop stays natural — most data flows through R lists, which R functions understand natively
- `car`/`cdr` work uniformly across both representations, so user code doesn't need to care
- Cons cells are only allocated when actually needed (improper pairs), keeping the common case fast

**Cons**:
- Two representations means every list-processing function in the stdlib must handle both (or delegate to `car`/`cdr`)
- `base::is.list()` returns `FALSE` for R calls (quoted forms), which can surprise R interop code — `list?` in Arl returns `#t` for calls, but R's `is.list` does not
- The `_as-list` utility (from `_utils.arl`) is needed throughout the stdlib to normalize calls to lists before R operations

## Reference Files

- `R/cells.R` — Cons and Promise R6 classes
- `inst/arl/list.arl` — car, cdr, cons, and composed accessors
- `inst/arl/types.arl` — list?, pair?, list-or-pair?, null? predicates
- `R/parser.R` — dotted-pair parsing (in `parse_list`)
- `R/compiler.R` — Cons decomposition in lambda params (in `compile_lambda`)
- `R/engine.R` — Cons builtin installation (in `install_builtins`)
