# Arl Execution Pipeline: Hot Path Analysis

## The Pipeline

```
Source text -> Tokenize -> Parse -> Macro Expand -> Compile -> R eval()
```

Entry point: `Engine$eval_text` (`R/engine.R:167`) -> `self$read(text)` -> `self$eval(...)`.

From benchmarks: tokenization and parsing dominate runtime on realistic workloads.
Macro expansion, compilation, and R's eval take comparable shares and are much
faster than tokenization/parsing.

---

## Phase 1: Tokenization (Slowest)

**Three sub-phases in `R/tokenizer.R`:**

1. **Regex matching** (lines 138-148): Single C-level `gregexpr(.TOKEN_PATTERN, source)`
   -- fast.
2. **Line/column computation** (lines 151-159): Vectorized `findInterval` -- fast.
3. **Classification loop** (lines 163-256): **This is the bottleneck.**

The R-level `for` loop processes every token:

- `substr(txt, 1L, 1L)` per token (function call overhead).
- If/else chain for type classification.
- **`list(type=..., value=..., line=..., col=...)` allocation per token** -- named
  list creation involves allocation + names vector + string hashing, thousands of
  times per file.
- `.classify_atom()` (lines 51-117) runs **4 `grepl` regex calls** per symbol token
  before falling through to the SYMBOL default. Most tokens are symbols, so all 4
  regexes execute and fail.

---

## Phase 2: Parsing (Second Slowest)

**Recursive descent in `R/parser.R`:**

- **`sugar_map` allocated fresh on every `parse_expr()` call** (lines 35-40) -- a
  4-element named list, created thousands of times.
- **`make_src` closure allocated per call** to `parse_expr`.
- **`tracker$src_new()`** creates a `structure(list(...), class = "arl_src")` per
  expression -- R6 method dispatch + named list + class attribute.
- **`grepl("/", token$value, fixed=TRUE)`** on every symbol token to check for
  qualified names.
- **O(n^2) top-level expression accumulation**: `expressions <- c(expressions,
  list(expr))` copies the list each iteration.
- S-expression parsing uses chunked collection (good), but still `c(elements,
  chunk)` at chunk boundaries.

---

## Phase 3: Macro Expansion

**`R/macro.R`, entry at line 57:**

- **Fast path** (lines 62-68): `get_all_macro_names` (cached by env identity) +
  `contains_macro_head` (recursive AST walk checking every call head). When no macros
  apply, this is pure overhead -- but it's one tree walk.
- **When `preserve_src=FALSE`**: `strip_src` does a *second* full recursive tree walk
  removing `arl_src` attributes.
- **Full expansion path**: For each macro call, performs 3 recursive tree walks --
  hygienize, hygiene unwrap, and src_inherit -- plus the macro function invocation
  itself and re-expansion of the result.
- `is_macro_impl` walks the env chain (5+ envs deep) per potential macro call.

---

## Phase 4: Compilation

**`R/compiler.R`, main dispatch at line 241:**

- `switch(op_name, ...)` for ~15 special forms (R's `switch` is linear scan, but
  small).
- **`as.call(list(...))` per AST node** -- list allocation + call object conversion.
- **`try_constant_fold`** (lines 1370-1429): `op_name %in% pure_functions` does
  O(30) scan per function application.
- **`src_inherit`/`src_get`/`src_set`** called per expression -- R6 method overhead +
  `attr()` access.
- Lambda compilation is the most complex: builds `as.function(list(params..., body),
  envir = .__env)` expressions, checks for tail-call optimization.

---

## Phase 5: R Evaluation

**`R/runtime.R:323`, `eval_compiled`:**

Per-evaluation overhead:

- `arl_env$push_env(env)` -- integer-to-string conversion for env key.
- `install_helpers` fast path -- 2x `exists`/`get0` with `inherits=FALSE` (C-level
  hash lookup, fast).
- `withVisible(eval(compiled_expr, envir = env))` -- the actual R eval.

Inside R's eval, the main runtime helpers are:

- **`.__true_p`** (byte-compiled): Called for every `if` test -- checks `is.null`,
  `isFALSE`, then 4-condition numeric check.
- **`.__define`** (byte-compiled): Called for every `define` -- `exists` +
  `bindingIsActive` check even though active bindings are rare.

Per-expression pipeline in `Engine$eval` (lines 120-162): 6 R6 method calls per
expression (macroexpand, compile, src_get, push, eval_compiled, pop) +
`withVisible` wrapper + final `strip_src` recursive walk on the result.

---

## Summary Table

| Phase | Cost per unit | Main culprits |
|---|---|---|
| **Tokenization** | Per token | Named list alloc, `.classify_atom` 4x grepl cascade, `substr` |
| **Parsing** | Per expression | `sugar_map` + closure alloc per call, `src_new` alloc, `grepl("/",...)` per symbol, O(n^2) top-level append |
| **Macro expansion** | Per AST | Full tree walk even when no macros apply, 3 recursive walks per expansion |
| **Compilation** | Per AST node | `as.call(list(...))` alloc, constant-fold whitelist scan, `src_inherit` per node |
| **R eval** | Per expression | `push_env`/`pop_env` string keys, `.__true_p` per if, `.__define` per define, 6 R6 calls per expr |

---

## Low-Hanging Optimization Opportunities

1. **`sugar_map` in parser** -- hoist to module-level constant (allocated once instead
   of thousands of times).
2. **`.classify_atom` grepl cascade** -- replace with single-char checks or a single
   combined regex.
3. **Per-token named list allocation** -- could use a column-oriented representation
   (parallel vectors of types, values, lines, cols) instead of list-of-lists.
4. **`contains_macro_head` walk** -- could be skipped when macro cache is empty
   (already happens, but `strip_src` still walks the tree).
5. **O(n^2) top-level expression append** -- preallocate or use chunked collection
   like s-expr parsing does.
6. **`try_constant_fold` whitelist** -- use an environment for O(1) lookup instead of
   `%in%`.
