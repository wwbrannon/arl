# Efficiency Improvement Opportunities for Rye Compiler & Macro Expander

## Context

This analysis identifies efficiency improvement opportunities in the Rye compiler (`R/compiler.R`) and macro expander (`R/macro.R`). The request specifically mentions suspicions about caching opportunities in macro expansion/compilation, but this analysis covers the full spectrum of optimization opportunities.

**Key Context:**
- Rye is a Lisp-like language implemented on R
- Recent optimizations (Feb 2026) have already addressed several O(n²) allocation issues
- Comprehensive benchmark infrastructure exists (`benchmarks/`) with baseline measurements
- The evaluator currently dominates execution time (60-80%), but compiler/macro overhead can be 30-50% for macro-heavy code
- 871 passing tests provide strong validation coverage

## Summary of Findings

The exploration identified **three major categories** of optimization opportunities:

1. **Caching & Memoization** (30-50% potential speedup for macro-heavy code) - HIGHEST IMPACT
2. **Allocation & Data Structure Optimizations** (20-30% potential speedup) - HIGH IMPACT
3. **Algorithmic & Code Quality Improvements** (10-20% potential speedup) - MEDIUM IMPACT

---

## Category 1: Caching & Memoization Opportunities

### 1.1 Macro Expansion Cache (HIGHEST PRIORITY)
**Status:** Already documented in TODO (item 1), not yet implemented

**Problem:**
- Every expression goes through `macroexpand()` before compilation (R/rye-engine.R:452)
- Identical macro expansions are computed repeatedly
- No caching mechanism exists for expanded forms
- Module loading repeatedly expands the same standard library macros

**Current Behavior:**
```r
# R/macro.R:36-50
macroexpand = function(expr, env, preserve_src) {
  # Gets macro registry EVERY TIME
  registry <- private$macro_registry(target_env, create = FALSE)
  macro_names <- ls(registry, all.names = TRUE)  # Enumerates ALL macros
  # ... then does full expansion
}
```

**Opportunity:**
Implement memoization cache with:
- **Key:** `(expr_hash, macro_registry_version)`
- **Value:** `expanded_form + src_metadata`
- **Invalidation:** Increment registry version on `defmacro()`

**Expected Impact:**
- 5-10x speedup for macro-heavy code
- 3-5x speedup for module loading (stdlib imports)
- Near-zero overhead for cache misses (simple hash lookup)

**Implementation Complexity:** Medium
- Add cache field to MacroExpander R6 class
- Implement simple hash-based lookup
- Track registry version for invalidation
- ~50-100 lines of code

**Critical Files:**
- `R/macro.R` (MacroExpander class)

---

### 1.2 Compilation Cache
**Status:** Already documented in TODO (item 2), not yet implemented

**Problem:**
- Every expression is compiled from scratch
- Identical expanded ASTs are recompiled repeatedly
- No caching between REPL evaluations
- Module bodies compiled on every load

**Opportunity:**
Cache compiled R expressions by expanded AST:
- **Key:** `(expanded_ast_hash, compiler_options)`
- **Value:** `compiled_r_expression`
- **Shared with:** Macro expansion cache for two-level caching

**Expected Impact:**
- 20-30% speedup for REPL usage
- 40-60% speedup for module reloading
- Compounds with macro cache for full pipeline optimization

**Implementation Complexity:** Medium
- Add cache field to RyeCompiler R6 class
- Hash expanded AST (need robust hashing)
- ~50-100 lines of code

**Critical Files:**
- `R/compiler.R` (RyeCompiler class)

---

### 1.3 Macro Registry Name Cache
**Status:** Not documented, newly identified

**Problem:**
```r
# R/macro.R:40 - Called EVERY macroexpand()
macro_names <- ls(registry, all.names = TRUE)
```
- `ls()` enumerates all bindings in registry environment - O(n) where n = number of macros
- Called on every single `macroexpand()` invocation
- Result is discarded and recomputed next time
- Standard library has ~20-30 macros, user code can add many more

**Opportunity:**
Cache macro names in MacroExpander R6 field:
- Update cache on `defmacro()` calls
- Avoid repeated `ls()` overhead
- Store as sorted vector for fast `match()` or convert to environment for O(1) lookup

**Expected Impact:**
- 10-15% speedup for frequent macro expansion
- O(n) → O(1) for macro existence checks
- Particularly beneficial with large macro registries

**Implementation Complexity:** Low
- Add `private$macro_name_cache` field
- Update in `define_macro()` method
- Clear on environment changes
- ~20-30 lines of code

**Critical Files:**
- `R/macro.R:138-140` (macro_registry method)
- `R/macro.R:36-50` (macroexpand method)

---

### 1.4 Macro Lookup Optimization
**Status:** Not documented, newly identified

**Problem:**
```r
# R/macro.R:157 - Used in contains_macro_head()
if (!is.na(match(op_name, macro_names))) { ... }
```
- Uses vector `match()` for symbol lookup - O(n) per symbol
- Called recursively for every symbol in expression tree
- O(expr_size * n_macros) total complexity

**Opportunity:**
Replace character vector with environment-based hash set:
- Convert `macro_names` vector to environment
- Use `exists(op_name, envir=macro_set)` for O(1) lookup
- Reduces `contains_macro_head()` from O(size * macros) to O(size)

**Expected Impact:**
- 20-30% speedup for expressions with many symbols
- More beneficial as macro count grows
- Enables scaling to large macro libraries

**Implementation Complexity:** Low
- Create hash set in `macroexpand()`
- Simple conversion: `as.environment(as.list(setNames(rep(TRUE, length(names)), names)))`
- ~10-20 lines of code

**Critical Files:**
- `R/macro.R:141-176` (contains_macro_head method)

---

## Category 2: Allocation & Data Structure Optimizations

### 2.1 Function Application Argument Collection (HIGH PRIORITY)
**Status:** Not fixed, still using old pattern

**Problem:**
Two nearly identical blocks of code with O(n²) allocation pattern:

```r
# R/compiler.R:861-886 (subscript operators)
args <- list()
i <- 2
while (i <= length(expr)) {
  # ...
  args <- c(args, list(val))  # O(n²) !!!
  names(args)[length(args)] <- keyword_name
  i <- ...
}

# R/compiler.R:895-920 (regular function calls)
args <- list()
i <- 2
while (i <= length(expr)) {
  # ... IDENTICAL LOGIC ...
  args <- c(args, list(val))  # O(n²) !!!
  names(args)[length(args)] <- keyword_name
  i <- ...
}
```

**Impact:**
- O(n²) complexity for functions with many arguments
- Code duplication (~60 lines)
- Pattern inconsistent with recent optimizations elsewhere (commits e51edc8, 25d4db7)

**Opportunity:**
1. **Preallocate:** `args <- vector("list", length(expr) - 1)`
2. **Index assignment:** `args[[arg_idx]] <- val; arg_idx <- arg_idx + 1`
3. **Extract to helper:** Eliminate code duplication

**Expected Impact:**
- 30-50% speedup for calls with 10+ arguments
- 2-3x speedup for calls with 50+ arguments
- Better code maintainability

**Implementation Complexity:** Low
- Follow pattern already established at R/compiler.R:68, 237
- Extract shared helper function
- ~50 lines changed/removed

**Critical Files:**
- `R/compiler.R:853-922` (compile_application method)

---

### 2.2 Logical Operators (and/or) Argument Collection
**Status:** Not fixed, still using old pattern

**Problem:**
```r
# R/compiler.R:626-695
compile_and = function(expr) {
  args <- list()
  for (i in 2:length(expr)) {
    compiled <- private$compile_impl(expr[[i]])
    args <- c(args, list(compiled))  # O(n²) !!!
  }
  # ... 34 more lines ...
}

compile_or = function(expr) {
  args <- list()
  for (i in 2:length(expr)) {
    compiled <- private$compile_impl(expr[[i]])
    args <- c(args, list(compiled))  # O(n²) !!!
  }
  # ... IDENTICAL 34 lines ...
}
```

**Impact:**
- O(n²) for multi-clause boolean expressions
- 138 lines of near-identical code (!!!)
- Inconsistent with recent preallocation optimizations

**Opportunity:**
1. **Preallocate:** `args <- vector("list", length(expr) - 1)`
2. **Merge implementations:** Extract common code with direction parameter
3. **Follow established pattern** from R/compiler.R:68, 237

**Expected Impact:**
- 20-40% speedup for long and/or chains
- Significant code reduction (~70 lines saved)
- Better maintainability

**Implementation Complexity:** Low
- Straightforward preallocation
- Simple refactoring to merge implementations
- ~100 lines changed/removed

**Critical Files:**
- `R/compiler.R:626-695` (compile_and and compile_or methods)

---

### 2.3 Lambda Body Parts Accumulation
**Status:** Not fully optimized

**Problem:**
```r
# R/compiler.R:380-407
body_parts <- list(env_bind)
if (params$has_rest && !is.null(params$rest_param)) {
  body_parts <- c(body_parts, list(...))  # Concat
}
if (length(params$param_bindings) > 0) {
  for (binding in params$param_bindings) {
    body_parts <- c(body_parts, list(...))  # Repeated concat in loop
  }
}
body_parts <- c(body_parts, compiled_body)  # Final concat
```

**Impact:**
- Multiple `c()` concatenations accumulate O(n²) cost
- More allocations than necessary
- Size is predictable and could be preallocated

**Opportunity:**
Preallocate with known size:
```r
# Size = 1 (env_bind) + (has_rest ? 1 : 0) + param_bindings + compiled_body
size <- 1 + as.integer(params$has_rest && !is.null(params$rest_param)) +
        length(params$param_bindings) + length(compiled_body)
body_parts <- vector("list", size)
idx <- 1
body_parts[[idx]] <- env_bind; idx <- idx + 1
# ... indexed assignment ...
```

**Expected Impact:**
- 10-20% speedup for lambdas with many parameter bindings
- Better allocation patterns

**Implementation Complexity:** Low-Medium
- Size calculation slightly complex but straightforward
- ~30 lines changed

**Critical Files:**
- `R/compiler.R:380-407` (compile_lambda method)

---

### 2.4 Early-Exit Loops in compile_begin and compile_lambda
**Status:** Not fixed in some locations

**Problem:**
```r
# R/compiler.R:291 (compile_begin)
compiled <- lapply(parts, function(e) private$compile_impl(e))
if (any(vapply(compiled, is.null, logical(1)))) {
  return(NULL)
}

# R/compiler.R:374 (compile_lambda body)
compiled_body <- lapply(body_exprs, function(e) private$compile_impl(e))
if (any(vapply(compiled_body, is.null, logical(1)))) {
  return(private$fail("lambda body could not be compiled"))
}
```

**Impact:**
- Allocates full list even if first element fails
- No early exit on compilation failure
- Unnecessary work for expressions that will fail

**Opportunity:**
Use imperative loop with early exit (pattern already used in `compile_seq`):
```r
compiled <- vector("list", length(parts))
for (i in seq_along(parts)) {
  compiled[[i]] <- private$compile_impl(parts[[i]])
  if (is.null(compiled[[i]])) {
    return(NULL)  # Early exit
  }
}
```

**Expected Impact:**
- 15-25% speedup when compilation fails (common in development)
- Faster failure detection
- Consistent with optimized compile_seq pattern

**Implementation Complexity:** Low
- Simple conversion to imperative loop
- Pattern already established in compile_seq
- ~20 lines changed

**Critical Files:**
- `R/compiler.R:286-295` (compile_begin method)
- `R/compiler.R:374` (compile_lambda method)

---

## Category 3: Algorithmic & Code Quality Improvements

### 3.1 Reduce Hygienization Cost
**Status:** Not documented, newly identified

**Problem:**
```r
# R/macro.R:980
result <- private$hygienize_expr(expanded, context)
```
- Every macro expansion hygienizes the ENTIRE result tree
- Large cost for macros that expand to big expressions
- Many nodes don't actually need hygienization (literals, quoted forms)

**Opportunity:**
Implement selective or incremental hygienization:
- Track which symbols are "introduced" vs "inherited" during expansion
- Only hygienize introduced symbols
- Skip hygienization for literals, quoted forms, inherited symbols
- Potentially lazy hygienization (delay until symbol actually used)

**Expected Impact:**
- 20-40% speedup for macros with large expansion results
- Particularly beneficial for code generation macros
- More complex to implement correctly

**Implementation Complexity:** High
- Requires careful analysis of hygiene requirements
- Risk of breaking variable capture prevention
- Needs comprehensive testing
- ~200-300 lines of code

**Critical Files:**
- `R/macro.R:957-1057` (macroexpand_impl method)
- `R/macro.R:465-570` (hygienize_expr and related methods)

**Recommendation:** Defer until after simpler optimizations, requires careful design

---

### 3.2 Quasiquote Evaluation Overhead
**Status:** Not documented, newly identified

**Problem:**
```r
# R/macro.R:622, 642 (in quasiquote_impl)
result <- private$eval_compiled_in_env(expr, env)
```
- Every unquote expression triggers full compile + eval cycle
- Can be expensive for macros with many unquotes
- Recursive macro expansion possible (unquote contains macros)

**Opportunity:**
- Cache compiled unquote expressions within same quasiquote expansion
- Lazy evaluation: delay unquote until value actually needed
- Detect and optimize common unquote patterns

**Expected Impact:**
- 15-25% speedup for macros with many unquotes
- Beneficial for template-heavy macros

**Implementation Complexity:** Medium-High
- Requires caching within quasiquote context
- Interaction with macro expansion needs careful handling
- ~100-150 lines of code

**Critical Files:**
- `R/macro.R:613-665` (quasiquote_impl method)

**Recommendation:** Medium priority, worthwhile after basic caching implemented

---

### 3.3 Source Metadata Optimization
**Status:** Not documented, newly identified

**Problem:**
- `src_inherit()` and `strip_src()` called repeatedly throughout compilation and expansion
- Extra allocation and attribute manipulation on every node
- Overhead for each recursive step

**Opportunity:**
- Defer source attachment until after full expansion
- Batch source metadata operations
- Flag-based accumulation instead of per-node manipulation

**Expected Impact:**
- 5-10% overall speedup
- Reduced allocation churn

**Implementation Complexity:** Medium
- Requires redesigning source tracking approach
- Risk of losing source information in error messages
- ~150-200 lines changed

**Recommendation:** Low priority, small benefit vs complexity

---

### 3.4 Module Load Cache
**Status:** Already documented in TODO (item 3)

**Problem:**
- Modules are parsed, macro-expanded, and compiled on every load
- No on-disk caching of compiled module bodies
- Particularly wasteful for standard library modules

**Opportunity:**
Cache compiled module bodies by file hash:
- Check `.rye_cache/` directory for compiled version
- Load from cache if file hash matches
- Skip parse/expand/compile steps entirely

**Expected Impact:**
- 60-80% speedup for module loading
- Particularly beneficial for standard library imports
- Faster REPL startup

**Implementation Complexity:** Medium-High
- Need reliable file hashing
- Cache invalidation on source changes
- Cross-session persistence
- ~200-300 lines of code

**Critical Files:**
- `R/modules.R` (module loading infrastructure)

**Recommendation:** High value for developer experience, medium priority

---

### 3.5 Constant Folding & Literal Hoisting
**Status:** Already documented in TODO (item 4)

**Problem:**
- Pure literal expressions like `(+ 1 2)` computed at runtime
- Immutable literals recreated on each evaluation
- Opportunities for compile-time evaluation

**Opportunity:**
- Detect and precompute pure literal calls during compilation
- Hoist immutable values into temporary variables
- Inline simple constant expressions

**Expected Impact:**
- 5-15% speedup for numeric-heavy code
- Better generated code quality

**Implementation Complexity:** Medium
- Need purity analysis for functions
- Risk of incorrect optimization
- ~200-250 lines of code

**Recommendation:** Medium-low priority, nice-to-have optimization

---

## Optimization Priority Ranking

### Tier 1: Highest Impact, Lowest Risk (Implement First)

1. **Macro Expansion Cache** (1.1)
   - Expected: 5-10x for macro-heavy code
   - Complexity: Medium
   - Risk: Low (clear invalidation strategy)
   - Already documented in TODO

2. **Function Application Argument Preallocation** (2.1)
   - Expected: 30-50% for multi-arg functions
   - Complexity: Low
   - Risk: Very low (pattern already established)
   - Code quality win (removes duplication)

3. **Macro Registry Name Cache** (1.3)
   - Expected: 10-15% for frequent expansion
   - Complexity: Low
   - Risk: Low
   - Quick win

4. **Logical Operators Preallocation & Merge** (2.2)
   - Expected: 20-40% for boolean chains
   - Complexity: Low
   - Risk: Very low
   - Code quality win (removes 70 lines of duplication)

### Tier 2: High Impact, Medium Complexity (Implement Second)

5. **Compilation Cache** (1.2)
   - Expected: 20-60% for REPL/modules
   - Complexity: Medium
   - Risk: Low
   - Synergizes with macro cache

6. **Macro Lookup Optimization** (1.4)
   - Expected: 20-30% for symbol-heavy expressions
   - Complexity: Low
   - Risk: Low
   - Enables scaling to large macro sets

7. **Early-Exit Loops** (2.4)
   - Expected: 15-25% on failures
   - Complexity: Low
   - Risk: Very low
   - Better developer experience

8. **Lambda Body Parts Preallocation** (2.3)
   - Expected: 10-20% for complex lambdas
   - Complexity: Low-Medium
   - Risk: Low

### Tier 3: Medium Impact, Higher Complexity (Consider Later)

9. **Module Load Cache** (3.4)
   - Expected: 60-80% for module loads
   - Complexity: Medium-High
   - Risk: Medium (cross-session persistence)
   - Great for developer experience

10. **Quasiquote Evaluation Optimization** (3.2)
    - Expected: 15-25% for unquote-heavy macros
    - Complexity: Medium-High
    - Risk: Medium

### Tier 4: Lower Priority (Future Work)

11. **Reduce Hygienization Cost** (3.1)
    - Expected: 20-40% for large macro expansions
    - Complexity: High
    - Risk: High (correctness concerns)

12. **Constant Folding** (3.5)
    - Expected: 5-15% for numeric code
    - Complexity: Medium
    - Risk: Medium

13. **Source Metadata Optimization** (3.3)
    - Expected: 5-10% overall
    - Complexity: Medium
    - Risk: Medium (error reporting impact)

---

## Implementation Recommendations

### Quick Wins (1-2 hours each)
Start with these for immediate impact:
- 2.1: Function application preallocation (LOW HANGING FRUIT)
- 2.2: Logical operators preallocation & merge (LOW HANGING FRUIT)
- 1.3: Macro registry name cache
- 1.4: Macro lookup optimization
- 2.4: Early-exit loops

**Total expected impact:** 30-50% speedup on compilation, minimal risk

### Medium Effort (4-8 hours each)
These provide the biggest performance gains:
- 1.1: Macro expansion cache (HIGHEST PRIORITY)
- 1.2: Compilation cache
- 2.3: Lambda body preallocation

**Total expected impact:** 5-10x for macro-heavy code, 2-3x for typical code

### Larger Projects (1-2 days each)
Consider after quick wins:
- 3.4: Module load cache
- 3.2: Quasiquote optimization

---

## Verification Strategy

All optimizations should be verified using:

1. **Existing Test Suite:** All 871 tests must pass
   - Run: `testthat::test_dir("tests/testthat")`

2. **Benchmark Suite:** Measure before/after performance
   - Component benchmarks: `benchmarks/bench-compiler.R`, `benchmarks/bench-macro.R`
   - End-to-end: `benchmarks/bench-e2e.R`
   - Real workloads: `benchmarks/bench-examples.R`

3. **Example Validation:** Real-world correctness
   - `inst/examples/fibonacci.rye`
   - `inst/examples/macro-examples.rye`
   - `inst/examples/quicksort.rye`

4. **Profiling:** Use `profvis` to verify hotspot reduction
   - Before/after profiles for each optimization
   - Verify expected impact materializes

---

## Summary

**Total Identified Opportunities:** 13 distinct optimizations

**Expected Impact Range:**
- Quick wins (Tier 1): 30-50% immediate speedup, 4-5 optimizations
- High-value caching (1.1, 1.2): 5-10x for macro-heavy code
- Combined potential: 3-5x overall speedup for typical Rye code

**Recommended Focus:**
Start with Tier 1 optimizations (low risk, immediate impact), then implement macro expansion cache (highest value). This provides 80% of the benefit with 20% of the effort.

**Key Files to Modify:**
- `R/compiler.R` (924 lines) - Most allocation optimizations
- `R/macro.R` (1059 lines) - All caching optimizations
- Comprehensive test coverage ensures safety

All recommendations are based on established patterns already present in the codebase (recent preallocation commits) and documented TODO items.

---

## Implementation Plan

**Phase 1 (This Session):** Implement all Tier 1 optimizations
- 2.1: Function application preallocation & deduplication
- 2.2: Logical operators preallocation & merge
- 1.3: Macro registry name cache
- 1.4: Macro lookup optimization
- 2.4: Early-exit loops

After Phase 1: Test, benchmark, profile, and commit

**Phase 2 (Next Session):** Implement Tier 2 optimizations
**Phase 3 (Future):** Implement Tier 3 & 4 as needed
