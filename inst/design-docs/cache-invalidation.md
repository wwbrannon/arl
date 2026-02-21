# Module Cache Invalidation Strategy

## Overview

Arl implements a code cache for module loading with content-based invalidation:
- **code cache**: Compiled expressions cache (`.code.rds`)
- **Inspection**: Human-readable code (`.code.R`) - for debugging

This document describes the cache invalidation strategy.

## Core Mechanism: Content-Based Hashing

### Cache Key Format
```
<filename>.<content_hash>.<type>
```

Example: `list.arl.a651f6218b6e130f.code.rds`

### Hash Algorithm
- **Algorithm**: MD5 (via `tools::md5sum`)
- **Input**: Raw file content (bytes)
- **Properties**: Deterministic, fast, collision-resistant

### Implementation

```r
get_cache_paths <- function(src_file) {
  # Hash raw file content for speed and simplicity
  file_hash <- tools::md5sum(src_file)

  # Cache files include hash in name
  list(
    code_cache = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.rds")),
    code_r = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.R"))
  )
}
```

## Validation Checks

When loading a cache file, two checks are performed:

### 1. Arl Version Match
```r
cache_data$version == packageVersion("arl")
```
- Ensures cache is from same Arl version
- Prevents incompatibilities from compiler changes
- Version changes invalidate all caches

### 2. File Content Hash Match
```r
cache_data$file_hash == computed_hash
```
- Detects any changes to file content
- Independent of file metadata (mtime, permissions)
- Comments, whitespace, formatting all matter (simple and correct)

## Key Properties

### Robust to Filesystem Operations
- `git checkout` changing mtimes -> No false invalidation
- `touch` command -> No false invalidation
- `rsync`/`cp` operations -> Works correctly
- Cross-platform file transfers -> Hash remains valid

### Exact Content Equality
- Any file change (comments, whitespace, code) -> Cache invalidated
- Simple and correct - no special cases
- Fast - no parsing or normalization overhead

### Collision-Free
- Hash included in filename -> Different content = different file
- Multiple versions can coexist in cache directory
- Reverting changes automatically reuses original cache

### No Mtime Issues
**Test Results:**
```
Mtime changed? TRUE
Hash changed? FALSE
Hash is mtime-independent
```

## Dependency Handling

**Why the code cache works without explicit dependency tracking:**

1. The **code cache** uses dynamic lookups at runtime
2. When dependency changes, its own cache is invalidated
3. Dependent modules automatically use new version on next load

**Example:**
```
Module A imports Module B
Step 1: B changes -> B's hash changes -> B's cache invalidated
Step 2: A loads from cache -> compiled code: import(B) + call(B$func)
Step 3: B loads fresh (new version) -> A gets new B automatically
```

## Performance Characteristics

### Hash Computation Cost
- MD5 via `tools::md5sum`: fast for typical module sizes
- Typical module (12KB): sub-millisecond per file
- For 20 modules: negligible total overhead
- Negligible compared to cache load time
- Negligible compared to full compile time

### Cache Lookup Flow
1. Compute hash of source file via `tools::md5sum`
2. Check if `.cache/<file>.<hash>.code.rds` exists -> O(1)
3. If yes, load and validate version
4. If no, full compilation

### Storage
- All caches stored under `tools::R_user_dir("arl", "cache")/modules/`
- Each source directory gets a unique subdirectory keyed by `<basename>-<dir_hash>`
- Multiple versions can coexist in cache directory
- Clean with `make clean-cache` or `unlink(file.path(tools::R_user_dir("arl", "cache"), "modules"), recursive = TRUE)`

## Test Results

### Content Hash Behavior
```
Content: val=42, Hash: 01d5ccdc6bc14b6c
Content: val=99, Hash: c864c386280ffc33
Content: val=42, Hash: 01d5ccdc6bc14b6c  <- Same as first!
```

### Validation Behavior
```
Validation with current file: TRUE
After content change: FALSE
After reverting: TRUE
After adding comment: FALSE  (any file change invalidates)
After reformatting: FALSE  (any file change invalidates)
```

### Performance Impact
```
First load (cache miss):  2.13s -> writes caches
Second load (cache hit):  0.27s -> reads from cache
Speedup:                  7.88x

Benchmark improvement:    ~20x for module-heavy workloads
```

## Edge Cases Handled

### Version Upgrade
- User upgrades Arl -> All caches invalidated
- Fresh compilation with new compiler
- Automatic on first run after upgrade

### File Replaced with Same Content
- Hash matches -> Cache valid
- No unnecessary recompilation

### Formatting Changes
- Any file modification -> Hash changes -> Cache invalidated
- Comments added/removed -> Cache invalidated
- Whitespace changes -> Cache invalidated
- This is acceptable: formatting changes are rare after initial development

### Concurrent Edits
- Different hash -> Different cache file
- No conflicts or corruption
- Multiple versions coexist

### Symbolic Links
- `tools::md5sum()` follows symlinks
- Hash based on target content
- Works correctly

## Comparison to Alternative Strategies

### Mtime-Based (Not Used)
- Breaks with `git checkout`
- False invalidation on `touch`
- Unreliable across network filesystems
- No semantic awareness

### Dependency Tracking (Not Needed)
- Complex to implement correctly
- Requires parsing all imports
- Transitive dependencies explode complexity
- Cache invalidation cascades
- Dynamic lookups make it unnecessary for Arl

### Content Hash (Chosen)
- Robust to filesystem operations
- Simple implementation
- No false positives or negatives
- Works with dynamic module system
- Very fast computation (~0.075ms per module)
- Exact equality - any file change invalidates (simple and correct)

## Implementation Files

- `R/module-cache.R` - Cache operations and validation
- `R/runtime.R` - Cache writing in `module_compiled()`
- `R/engine.R` - Cache reading in `load_file_in_env()`

## Related Documentation

- See `R/module-cache.R` for detailed implementation
- See `tests/` for cache invalidation tests
- See `*.code.R` files under `tools::R_user_dir("arl", "cache")/modules/` for human-readable compiled output (requires `options(arl.debug_cache = TRUE)`)

## Conclusion

**The content hash strategy is optimal for Arl because:**

1. **Simple and robust** - No complex dependency tracking required
2. **Very fast** - Hash computation is ~0.075ms per file
3. **Reliable** - Immune to filesystem quirks
4. **Works with dynamic modules** - No dependency tracking needed
5. **Exact equality** - Simple semantics, no edge cases
6. **Proven** - Comprehensive tests validate correctness

The combination of:
- Raw content hashing (fast, simple, correct)
- Version tracking
- Dynamic module loading

...provides optimal performance while maintaining correctness. Formatting/comment changes invalidate caches, but these are rare after initial development and the simplicity is worth the trade-off.
