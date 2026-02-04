# Test Issues: Critique of Encoded Behaviors

Based on a thorough review of the tests, several behaviors appear to be **bugs incorrectly encoded as expected behavior**.

---

## 1. quotient and remainder have wrong semantics

**The problem:** The implementation uses floor division instead of truncation toward zero.

**In `inst/rye/math.rye`:**

```rye
(define quotient
  (lambda (x y)
    (__ras.integer (__rfloor (__r/ x y)))))  ;; Uses floor, not truncate!
```

**The tests in `test-math-extended.rye` encode the wrong behavior:**

```rye
(define test-quotient-negative-numerator (lambda ()
  (assert-equal (quotient -17 5) -4)))  ;; Should be -3

(define test-remainder-negative-numerator (lambda ()
  (assert-equal (remainder -17 5) 3)))  ;; Should be -2
```

**Scheme semantics:**

- `(quotient -17 5)` → `-3` (truncate toward zero)
- `(remainder -17 5)` → `-2` (same sign as dividend)

**Current Rye behavior:**

- `(quotient -17 5)` → `-4` (floor toward -∞)
- `(remainder -17 5)` → `3` (wrong sign!)

The docstring for `remainder` even says "same sign as x" but the result `3` has the opposite sign of `-17`. The identity `x = y * quotient(x,y) + remainder(x,y)` holds, but both functions have the wrong semantics.

---

## 2. empty? docstring vs. behavior mismatch

**The docstring says:**

> "Return #t if x is an empty list, call, string, or vector."

**But the test in `test-stdlib-edge-cases.R` line 267:**

```r
expect_false(env$`empty?`(""))  # empty string returns FALSE!
```

The implementation checks `(length x)` (vector length) rather than string length. An empty string `""` is a character vector of length 1 in R, not length 0. If "empty string" is the intent, it should check `(nchar str)`.

---

## 3. vector? arbitrarily excludes character vectors

**From `predicates.rye`:**

```rye
(define vector?
  (lambda (x)
    (if (is.atomic x)
      (if (is.character x) #f ...))))  ;; Character vectors not vectors?
```

**And the test confirms:**

```r
expect_false(env$`vector?`(c("a", "b")))  # character vectors are not considered vectors
```

This is inconsistent with both R (where character vectors are vectors) and Scheme (where vectors can contain any type). The choice seems arbitrary.

---

## 4. Division by zero produces Inf instead of error

**Tests in `test-numbers.rye` encode R's behavior:**

```rye
(define test-division-by-zero-positive (lambda ()
  (define result (/ 1 0))
  (assert-true (is.infinite result))))
```

In standard Scheme, `(/ 1 0)` should raise an error. While adopting R's `Inf` semantics might be intentional for R interop, it's a significant departure from Scheme semantics that's worth documenting prominently.

---

## 5. NaN == NaN returns NA, not #f

```rye
(define test-nan-not-equal-self (lambda ()
  (define comparison (== NaN NaN))
  (assert-true (is.na comparison))))
```

IEEE 754 and most languages (including Scheme) specify `NaN == NaN` → `#f`. R's NA-propagation semantics are unusual. This could cause subtle bugs when porting code.

---

## Questionable but possibly acceptable

These seem like deliberate R-isms rather than bugs:

- **identical? comparing lists structurally** — The test acknowledges this is R behavior and uses `identical?` (not `eq?`) to make it clear.
- **eq? / eqv? throwing errors** — Actually the correct approach since R can't implement true Scheme identity semantics.
- **string->list returning list of strings** — R lacks a character type, so `("a" "b" "c")` is reasonable instead of `(#\a #\b #\c)`.
- **rational? accepting all finite numbers** — Computationally all floats are rational; the docstring acknowledges this.

---

## Recommendation

The most problematic issues are **quotient/remainder** since they're core numeric operations with clear Scheme specifications. The fix would be:

```rye
(define quotient
  (lambda (x y)
    (__ras.integer (__rtrunc (__r/ x y)))))  ;; Use trunc, not floor
```

The **empty?** inconsistency is also confusing — either fix the implementation to check string content or change the docstring to say "empty character vector."
