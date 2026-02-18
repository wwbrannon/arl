# Design Doc: Macro Hygiene System

## Problem

Defmacro-style macros suffer from accidental variable capture. Consider a macro that introduces a temporary binding `tmp` — if the caller also uses `tmp`, the macro silently shadows it. Scheme solves this with `syntax-rules`/`syntax-case`, but those require a pattern-based macro system. Arl uses procedural `defmacro` — how to add hygiene without losing flexibility?

## Decision

Automatic renaming of macro-introduced bindings via a post-expansion hygienization pass, with `capture` for explicit opt-out (anaphoric macros). This is closest to Clojure's auto-gensym approach but applied systematically to all binding forms.

## Alternatives Considered

| Approach | Pros | Cons |
|---|---|---|
| Raw defmacro (Common Lisp) | Simple, powerful | Requires manual `gensym`, error-prone |
| `syntax-rules` (Scheme R5RS) | Hygienic by construction | Pattern-only, no procedural code |
| `syntax-case` (Scheme R6RS) | Powerful + hygienic | Complex implementation, steep learning curve |
| **Arl: defmacro + auto-hygiene + `capture`** | Procedural flexibility, automatic safety | Implementation complexity in the hygienization pass |

## Origin Tracking

The hygiene system tracks where each piece of expanded code came from using `arl_syntax` S3 wrappers. Three origins:

- **`"call_site"`** — code from the caller, injected via unquote (`,`). Protected from renaming.
- **`"introduced"`** — code explicitly marked via `capture`. Also protected from renaming.
- **Implicit (no wrapper or unknown origin)** — code introduced by the macro body. Subject to renaming.

Wrappers are applied during quasiquote expansion. When a macro uses `` `(let ((tmp ,expr)) tmp) ``, the `,expr` is evaluated and its result wrapped with `"call_site"` origin, while `tmp` and `let` have no wrapper (macro-introduced).

### Implementation

```r
# R/macro.R:292-293
hygiene_wrap = function(expr, origin) {
  structure(list(expr = expr, origin = origin), class = "arl_syntax")
}
```

The `wrap_fn` passed to `quasiquote_expand` (in `R/quasiquote.R`) tags each unquoted value:

```r
# R/macro.R:124
wrap_fn <- function(val) private$hygiene_wrap(val, "call_site")
```

## The Hygienization Pass

After a macro function returns expanded code, `hygienize` walks the expression tree and renames macro-introduced bindings. The core logic is in `hygienize_expr` (`R/macro.R:356-445`).

### Renaming Rules

1. **Binding positions** in `define`, `lambda`, `let`/`let*`/`letrec` — if the name has macro origin (no wrapper), it gets renamed via `hygiene_gensym` (monotonic `__h` counter, e.g. `tmp` → `tmp__h3`)

2. **Call-site code** (origin `"call_site"`) — the entire subtree is processed with `protected = TRUE`, meaning no symbols in it are renamed

3. **Introduced code** (origin `"introduced"` from `capture`) — processed with `protected = FALSE`, so it participates in the *macro's* scope but isn't itself renamed

4. **Symbol references** — if a symbol appears in the substitution environment (meaning it was renamed in a binding position), the reference is replaced with the renamed version

### Scope Tracking

The pass maintains a substitution environment (`env`, an R named list) that maps original names to their `hygiene_gensym` replacements. This environment is threaded through recursive calls and extended at each binding site:

```r
# R/macro.R:470-474 (inside hygienize_define)
if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
  name <- as.character(name_expr)
  fresh <- private$hygiene_gensym(name)
  new_env[[name]] <- fresh
  result[[2]] <- fresh
}
```

### Form-Specific Handling

- **`begin`** — processes forms sequentially, threading the substitution env so that `define` in one form is visible in subsequent forms
- **`define`** — renames the bound name (if macro-introduced) and processes the value expression
- **`lambda`** — renames parameters (if macro-introduced), extends the substitution env, processes the body
- **`let`** — renames all bound names first, then processes values in the *outer* env and body in the *inner* env
- **`let*`** — renames sequentially, each binding's value sees prior bindings
- **`letrec`** — renames all bound names first, then processes all values in the *inner* env (mutual recursion)

## The `capture` Escape Hatch

For anaphoric macros — macros that intentionally introduce bindings for the caller to reference — `capture` re-tags symbols as `"introduced"`:

```scheme
;; Anaphoric if: binds `it` for the caller
(defmacro (aif test then else)
  (capture 'it
    `(let ((it ,test))
       (if it ,then ,else))))
```

Implementation in `capture_mark` (`R/macro.R:342-352`): walks the expression, finds symbols matching the given name, and wraps them with `"introduced"` origin. This prevents the hygienization pass from renaming them, so the caller can reference `it`.

## Cross-Module Macro Hygiene

When a macro defined in module A is used in module B, free variables from A's scope need resolution. The `hygienize` method accepts `defining_env` and `use_site_env` parameters:

```r
# R/macro.R:1007-1008 (in macroexpand_impl)
defining_env <- environment(macro_fn)$env
expanded <- private$hygienize(expanded, defining_env, use_site_env = env)
```

For unprotected symbols not in the substitution env, the pass checks if they exist in the defining module's env but NOT in the shared prelude/builtins chain. If so, the value is captured as an `arl_resolved_ref`:

```r
# R/macro.R:1-15
arl_resolved_ref <- function(value, source_symbol, module_name = NULL) {
  structure(
    list(value = value, source_symbol = source_symbol, module_name = module_name),
    class = "arl_resolved_ref"
  )
}
```

This ensures that a macro's internal helpers (defined in its module) are available at the use site without requiring the caller to import them. The optimization skips resolution when the use-site env has the identical binding.

## `gensym` vs `hygiene_gensym`

Two distinct symbol generators serve different purposes:

- **`gensym`** (user-facing, `R/macro.R:278-286`) — generates `G__1`, `G__2`, etc. Checks the environment for collisions before returning. Exposed as a builtin for manual hygiene in macro bodies.

- **`hygiene_gensym`** (internal, `R/macro.R:288-291`) — generates `H__h1`, `H__h2`, etc. Uses a monotonic counter with `__h` suffix, guaranteed unique without env checks. Used exclusively by the hygienization pass.

## End-to-End Flow

1. User writes `(my-macro arg1 arg2)` — the compiler sees a macro call
2. Macro expander calls the macro function with unevaluated args
3. Macro body executes, typically using quasiquote to build output
4. During quasiquote expansion, unquoted args (`,arg1`) are evaluated and wrapped with `"call_site"` origin
5. Macro returns expanded code containing `arl_syntax` wrappers
6. `hygienize` walks the expansion:
   - Binding forms with macro origin get fresh names via `hygiene_gensym`
   - Call-site subtrees pass through protected
   - `capture`-introduced symbols pass through unprotected (visible to caller)
   - Free variables from the defining module become `arl_resolved_ref` values
7. `hygiene_unwrap_impl` strips all `arl_syntax` wrappers from the result
8. Source tracking inherits location info from the original call
9. If the expansion contains more macro calls, the process recurses (`macroexpand_impl` loops)
10. Final expanded code is compiled normally

## Known Limitations

- **Destructuring patterns** in binding forms are handled statically, not fully hygienized (the `hygienize_define_pattern` path is simpler)
- **Origin wrappers don't persist through `quote`** — this is intentional, since quoted code is data, not code to be hygienized
- **Captured `arl_resolved_ref` values** snapshot the value at expansion time — they don't update if the defining module is reloaded
- **No recursive hygiene for complex nested patterns** — deeply nested `(pattern ...)` forms in macro parameters may not be fully renamed
- **The hygienization pass is not applied to `defmacro` bodies themselves** — only to their expansions. A macro that expands to a `defmacro` will have the inner macro's body hygienized when *that* macro is later expanded.

## Reference Files

- `R/macro.R:288-728` — hygiene_gensym, hygiene_wrap, hygienize and all form-specific handlers
- `R/macro.R:977-1009` — macroexpand_impl (where hygienize is called after expansion)
- `R/quasiquote.R:30-42` — unquote handling with wrap_fn for origin tagging
- `R/engine.R:696-698` — `capture` and `gensym` builtin installation
- `R/macro.R:1-24` — arl_resolved_ref for cross-module references
