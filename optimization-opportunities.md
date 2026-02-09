# Performance Optimization Opportunities

You've already captured the biggest wins: the compiler eliminates the CPS/trampoline overhead (huge), and disk-backed caching avoids re-parsing/compiling stdlib modules on each engine startup. The 85s-to-7.5s improvement is impressive. Here's what I see as remaining opportunities, organized by likely impact.

## Tier 3: Architectural / Longer-Term

### 12. ~~Tail-call optimization for self-recursion~~ (DONE)

Implemented in the compiler. `compile_define` detects `(define name (lambda ...))` patterns, passes `self_name` to `compile_lambda`, which analyzes tail positions and rewrites self-calls as `while(TRUE)` loops. Handles simple params, destructuring, rest params, pattern rest params, keyword args, and tail calls through `if`/`begin`/`cond`/`let`/`let*`/`letrec`.

## Summary by Priority

| Priority | Optimization | Expected Impact | Effort |
|----------|-------------|----------------|--------|
| 1 | Tokenizer: vector indexing instead of `substr()` | 3-5x tokenizer speedup | Low |
| 2 | Skip `install_helpers()` when already installed | Significant for module loading | Very low |
| 3 | Lazy `strip_src` (only at boundaries) | 10-20% overall | Medium |
| 4 | Avoid redundant `strip_src` in `.rye_assign_pattern` | 5-10% for define-heavy code | Very low |
| 5 | Single compiled block per module | Reduces per-expression overhead | Low-Medium |
| 6 | Regex-based tokenizer | 5-10x tokenizer speedup | Medium |
| 7 | Bulk module attach with `list2env` | Faster engine startup | Very low |
| 8 | Cache R6 method references in hot loops | 5-10% overall | Low |

Items 1-4 and 7 are the low-hanging fruit that could be done quickly. The tokenizer rewrite (#1 or #6) and lazy strip_src (#3) are probably the biggest remaining wins after what you've already done.
