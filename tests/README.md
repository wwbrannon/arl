# Arl Test Suite

This directory contains the test suite for the Arl language implementation. Tests are organized into two main categories: **R tests** and **native tests**.

## Test Organization

```
tests/
├── testthat/              # R-based tests using testthat framework
│   ├── test-*.R           # R test files
│   ├── helper-*.R         # R test helpers
│   └── helper-native.arl  # Arl helpers for native tests (skip, etc.)
├── native/                # Native Arl tests (.arl files)
│   └── test-*.arl         # Native test files
└── skip-examples.arl      # Example usage of skip() (not a test)
```

## R Tests (testthat/)

Traditional R tests using the testthat framework. These tests:
- Test the R implementation of the Arl engine
- Test R interop and integration
- Test stdlib functions from the R side
- Use standard testthat assertions (`expect_equal`, `expect_error`, etc.)

**Example:**
```r
test_that("lambda creates functions", {
  engine <- Engine$new()
  env <- new.env()
  result <- engine$eval_in_env(
    engine$read("((lambda (x) (* x 2)) 5)")[[1]], env)
  expect_equal(result, 10)
})
```

## Native Tests (native/)

Native Arl tests written in Arl itself. These tests:
- Test Arl language features and semantics from within Arl
- Test stdlib functionality using Arl assertions
- Provide examples of idiomatic Arl code
- Run faster than R tests for pure Arl logic

### How Native Tests Work

1. **Test files** are `.arl` files in `tests/native/` directory
2. **Test functions** are defined with names starting with `test-`
3. **Test runner** (`helper-native.R`) automatically discovers and runs all `test-*` functions
4. **Assertions** use functions from the `assert` module (`assert-equal`, `assert-true`, etc.)

### Writing a Native Test

Create a file `tests/native/test-feature.arl`:

```arl
;;; Tests for my feature

(define test-basic-functionality (lambda ()
  ;; Test that 1 + 1 = 2
  (assert-equal 2 (+ 1 1))))

(define test-edge-case (lambda ()
  ;; Test empty list behavior
  (assert-true (null? (list)))))

(define test-error-handling (lambda ()
  ;; Test that division by zero errors
  (assert-error (lambda ()
    (/ 1 0)))))
```

**Key points:**
- Each test is a function named `test-something`
- Use `assert-equal`, `assert-true`, `assert-false`, `assert-eq`, `assert-error`
- Tests run in a fresh environment with stdlib loaded
- Tests are isolated from each other

### Available Assertions

From the `assert` module (automatically available):

- `(assert condition [message])` - Assert condition is truthy
- `(assert-equal expected actual)` - Assert structural equality (uses `equal?`)
- `(assert-eq expected actual)` - Assert identity (uses `identical?`)
- `(assert-true value)` - Assert value is truthy
- `(assert-false value)` - Assert value is falsy
- `(assert-error thunk)` - Assert thunk throws an error

### Skipping Tests

Use `skip()` to mark a test as skipped (only available in native tests):

```arl
(define test-future-feature (lambda ()
  (skip "Not yet implemented")
  ;; Code after skip is never reached
  (assert-equal 1 2)))

(define test-platform-specific (lambda ()
  ;; Skip on Windows
  (define os (r/call "Sys.info" (list)))
  (if (== (r/call "[[" (list os "sysname")) "Windows")
    (skip "Not supported on Windows")
    (assert-equal 1 1))))

(define test-requires-new-r (lambda ()
  ;; Skip if R version too old
  (if (< (r/call "getRversion" (list)) (r/call "package_version" (list "4.5")))
    (skip "Requires R >= 4.5")
    (assert-equal 1 1))))
```

**Note:** `skip()` is defined in `testthat/helper-native.arl` and calls `testthat::skip()`, so it integrates with the R test framework. Skipped tests appear in test output but don't fail the suite.

### Test Infrastructure

Native tests are run by `helper-native.R`:

1. Creates a fresh `Engine` with stdlib loaded
2. Loads `helper-native.arl` to provide test utilities (like `skip`)
3. Discovers all `.arl` files in `native/`
4. Loads each file and finds all `test-*` functions
5. Runs each test inside a `test_that()` block
6. Catches errors and reports failures/skips

## Running Tests

```r
# Run all tests
devtools::test()

# Run only R tests
devtools::test(filter = "^(?!native)")

# Run only native tests
devtools::test(filter = "native")

# Run specific test file
testthat::test_file("tests/testthat/test-engine.R")
```

## Test Guidelines

### When to Write R Tests vs Native Tests

**Use R tests when:**
- Testing the R engine implementation
- Testing R interop features
- Testing integration with R packages
- Need to test R-specific edge cases

**Use native tests when:**
- Testing Arl language semantics
- Testing stdlib functions from user perspective
- Demonstrating idiomatic Arl patterns
- Testing pure Arl logic without R concerns

### General Guidelines

- Keep tests focused and independent
- Use descriptive test names that explain what's being tested
- Group related tests in the same file
- Add comments explaining non-obvious test logic
- Test both success and failure cases
- Consider edge cases (empty lists, null, zero, etc.)

## Avoiding Test Duplication

The test suite includes both R tests (`tests/testthat/`) and native tests (`tests/native/`), each serving different purposes. To avoid unnecessary duplication and maintenance burden, follow these principles:

### Native Tests Should

- **Demonstrate idiomatic Arl usage patterns** - Show how features are meant to be used
- **Test language semantics** - Focus on binding, scoping, closures, control flow, quoting
- **Show 1-3 simple examples per stdlib function** - Demonstrate happy path usage
- **Serve as executable documentation** - Be readable and exemplary
- **Be concise and clear** - Each test should illustrate a specific pattern

**Example of a good native test:**
```arl
(define test-string-upcase (lambda ()
  (assert-equal (string-upcase "hello") "HELLO")))
```

### Native Tests Should NOT

- **Test comprehensive edge cases** - Empty strings, NaN, boundary conditions belong in R tests
- **Verify implementation details** - Caching behavior, optimization, internal state
- **Duplicate detailed R test coverage** - If R tests comprehensively cover something, native tests should show only 1-2 examples
- **Test system integration** - Actual file I/O, R interop details
- **Test error handling exhaustively** - Basic error cases are OK, but comprehensive error testing belongs in R tests

### R Tests Should

- **Test R implementation correctness** - Engine, compiler, optimizer behavior
- **Test comprehensive edge cases and error handling** - All boundary conditions, error messages
- **Verify R interop and module system** - Integration with R, module loading
- **Test actual file I/O and system integration** - Real filesystem operations
- **Verify performance characteristics** - Memoization caching, optimization effects

**Example of a good R test:**
```r
test_that("string-upcase handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env)

  expect_equal(env$`string-upcase`("hello"), "HELLO")
  expect_equal(env$`string-upcase`(""), "")
  expect_equal(env$`string-upcase`("ALREADY"), "ALREADY")
  expect_error(env$`string-upcase`(NULL))
})
```

### Acceptable Overlap

- **Core language features** can be tested in both suites (different perspectives)
- **Important stdlib functions** can have basic native examples + comprehensive R tests
- The overlap should be **intentional, not accidental** - consider whether duplication adds value

### Before Writing a Test

1. **Check if it's already covered** - Search both `native/` and `testthat/` directories
2. **Ask: "What am I testing?"**
   - Language semantics or usage pattern → Native test
   - Implementation correctness or edge case → R test
3. **For stdlib functions:**
   - Native: 1-2 clear examples showing typical usage
   - R: Comprehensive edge cases, error handling, integration
4. **When in doubt**, prefer R tests for comprehensive testing, reserve native tests for demonstrating idiomatic patterns

## Debugging Test Failures

For R tests, use standard R debugging:
```r
testthat::test_file("tests/testthat/test-file.R")
# Set breakpoints, use browser(), etc.
```

For native tests, you can load and run them manually:
```r
engine <- Engine$new()
env <- engine$get_env()
source("tests/testthat/helper-native.R")  # Loads skip() etc.
engine$load_file("tests/native/test-something.arl", env)
# Now you can call test functions directly:
env$`test-my-function`()
```

## Contributing Tests

When adding new features:
1. Add R tests for the implementation
2. Add native tests demonstrating usage
3. Ensure all existing tests still pass
4. Consider edge cases and error conditions

When fixing bugs:
1. Add a test that reproduces the bug (should fail initially)
2. Fix the bug
3. Verify the test now passes
4. Ensure no other tests broke
