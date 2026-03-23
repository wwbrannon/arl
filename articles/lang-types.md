# Standard Library: Types, Equality, and Conversions

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## List and Pair Predicates

### list?

Return \#t if x is a proper list (R list or call), not a dotted pair.

**Signature:** `(list? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (list? (list 1 2 3))  ; => #t
    #> TRUE
    arl> (list? ())  ; => #t
    #> TRUE
    arl> (list? "hello")  ; => #f
    #> FALSE

**See also:** [pair?](#pair-p), [list-or-pair?](#list-or-pair-p),
[null?](#null-p)

------------------------------------------------------------------------

### list-or-pair?

Return \#t if x is a non-empty list or dotted pair (pairlist cell).

**Signature:** `(list-or-pair? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (list-or-pair? (list 1))  ; => #t
    #> TRUE

**See also:** [list?](#list-p), [pair?](#pair-p), [null?](#null-p),
[atom?](#atom-p)

------------------------------------------------------------------------

### null?

Return \#t for empty list or \#nil.

**Signature:** `(null? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (null? #nil)  ; => #t
    #> TRUE
    arl> (null? ())  ; => #t
    #> TRUE
    arl> (null? (list 1))  ; => #f
    #> FALSE

**See also:** [nil?](#nil-p), [pair?](#pair-p),
[list-or-pair?](#list-or-pair-p)

------------------------------------------------------------------------

### nil?

Alias for null?.

**Signature:** `(nil? x)`

**Parameters:** - **`x`** — Value to test

**See also:** [null?](#null-p)

------------------------------------------------------------------------

### atom?

Return \#t if x is not a non-empty list or dotted pair (i.e. not
list-or-pair?).

**Signature:** `(atom? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (atom? 42)  ; => #t
    #> TRUE
    arl> (atom? "hello")  ; => #t
    #> TRUE
    arl> (atom? (list 1 2))  ; => #f
    #> FALSE
    arl> (atom? ())  ; => #t  (empty list is atomic)
    #> TRUE

**See also:** [list-or-pair?](#list-or-pair-p)

------------------------------------------------------------------------

### empty?

Return \#t if x is empty (0-length). (Note the empty string “” has
length 1 and is not “empty”!)

**Signature:** `(empty? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (empty? ())  ; => #t
    #> TRUE
    arl> (empty? (list))  ; => #t
    #> TRUE
    arl> (empty? (list 1))  ; => #f
    #> FALSE

**Note:** The empty string `""` has length 1 in R and is NOT considered
empty!

**See also:** [null?](#null-p),
[length=](https://willbrannon.com/arl/articles/lang-list-seq.html#length-eq)
(in `sequences` module)

------------------------------------------------------------------------

### pair?

Return \#t if x is a cons cell (dotted pair).

**Signature:** `(pair? x)`

**Examples:**

    arl> (pair? (cons 1 2))        ; => #t (dotted pair)
    #> TRUE
    arl> (pair? (cons 1 (list 2 3)))  ; => #f (cons onto list returns a list)
    #> FALSE
    arl> (pair? '(1 2 3))          ; => #f (quoted list, not a cons cell)
    #> FALSE
    arl> (pair? '())               ; => #f
    #> FALSE

**See also:** [list?](#list-p), [null?](#null-p), [atom?](#atom-p)

------------------------------------------------------------------------

## Symbol Predicates

### symbol?

Return \#t if x is a symbol.

**Signature:** `(symbol? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (symbol? 'foo)  ; => #t
    #> TRUE
    arl> (symbol? "foo")  ; => #f
    #> FALSE
    arl> (symbol? 42)  ; => #f
    #> FALSE

------------------------------------------------------------------------

### keyword?

Return \#t if x is an Arl keyword.

**Signature:** `(keyword? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (keyword? ':foo)  ; => #t
    #> TRUE
    arl> (keyword? 'foo)   ; => #f
    #> FALSE

------------------------------------------------------------------------

## Basic Type Predicates

### number?

Return \#t if x is a number (real or complex).

**Signature:** `(number? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (number? 42)  ; => #t
    #> TRUE

**See also:** [real?](#real-p), [complex?](#complex-p),
[rational?](#rational-p), [exact?](#exact-p), [inexact?](#inexact-p)

------------------------------------------------------------------------

### string?

Return \#t if x is character.

**Signature:** `(string? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (string? "hello")  ; => #t
    #> TRUE

------------------------------------------------------------------------

### vector?

Return \#t if x is a non-list atomic vector.

**Signature:** `(vector? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (vector? (c 1 2 3))  ; => #t
    #> TRUE

**Note:** Tests whether x is a non-list atomic vector (numeric,
character, logical, etc.). R lists are NOT vectors by this predicate.

------------------------------------------------------------------------

### boolean?

Return \#t if x is a single logical value.

**Signature:** `(boolean? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (boolean? #t)  ; => #t
    #> TRUE
    arl> (boolean? #f)  ; => #t
    #> TRUE
    arl> (boolean? TRUE)  ; => #t
    #> TRUE
    arl> (boolean? (c TRUE FALSE))  ; => #f
    #> FALSE

------------------------------------------------------------------------

### true?

Return \#t if x is TRUE.

**Signature:** `(true? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (true? #t)  ; => #t
    #> TRUE

------------------------------------------------------------------------

### false?

Return \#t if x is \#f.

**Signature:** `(false? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (false? #f)  ; => #t
    #> TRUE

------------------------------------------------------------------------

## Function Predicates

### fn?

Return \#t if x is a function.

**Signature:** `(fn? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (fn? car)  ; => #t
    #> TRUE

**See also:** [callable?](#callable-p), [procedure?](#procedure-p)

------------------------------------------------------------------------

### callable?

Alias for fn?.

**Signature:** `(callable? x)`

**Parameters:** - **`x`** — Value to test

**See also:** [fn?](#fn-p)

------------------------------------------------------------------------

### procedure?

Alias for fn? - return \#t if x is a function.

**Signature:** `(procedure? x)`

**Parameters:** - **`x`** — Value to test

**See also:** [fn?](#fn-p), [callable?](#callable-p)

------------------------------------------------------------------------

## Environment Predicates

### environment?

Test if x is an environment (including R6 objects).

**Signature:** `(environment? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (environment? (baseenv))  ; => #t
    #> TRUE

**Note:** R6 objects, dicts, and sets are all environments in R, so this
predicate returns \#t for them.

------------------------------------------------------------------------

### is-refclass?

Test if x is a Reference Class object.

**Signature:** `(is-refclass? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (is-refclass? (baseenv))  ; => #f
    #> FALSE

------------------------------------------------------------------------

## Type Introspection

### type-of

Return the type of value (alias for R’s typeof).

**Signature:** `(type-of x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (type-of 42)  ; => "double"
    #> "double"

**See also:** class, mode (R functions)

------------------------------------------------------------------------

## Numeric Type Predicates

Arl implements a numeric tower adapted for R’s type system:

    number?    (is.numeric OR is.complex)
    ├─ complex?  (is.complex)
    └─ real?     (is.numeric AND NOT is.complex)
       ├─ ±Inf   (real but not rational)
       └─ rational? (real? AND is.finite)
          └─ integer? (is.finite AND is.numeric AND x == as.integer(x))
             └─ natural? (integer? AND x >= 0)

    Orthogonal predicates:
    exact?   (is.integer - storage type)
    inexact? (number? AND NOT is.integer)

### real?

Return \#t if x is a real number (includes ±Inf, excludes complex).

**Signature:** `(real? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (real? 42)            ; => #t
    #> TRUE
    arl> (real? 3.14)          ; => #t
    #> TRUE
    arl> (real? Inf)           ; => #t
    #> TRUE
    arl> (real? (make-rectangular 3 4))  ; => #f
    #> FALSE

**See also:** [rational?](#rational-p), [complex?](#complex-p),
[number?](#number-p)

------------------------------------------------------------------------

### complex?

Return \#t if x is a complex number.

**Signature:** `(complex? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (complex? (make-rectangular 3 4))  ; => #t
    #> TRUE
    arl> (complex? 42)         ; => #f
    #> FALSE

**See also:** [real?](#real-p), [number?](#number-p),
[make-rectangular](https://willbrannon.com/arl/articles/lang-math.html#make-rectangular)
(in `math` module)

------------------------------------------------------------------------

### rational?

Return \#t if x is a finite real number (rational in R are finite
floats).

**Signature:** `(rational? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (rational? 42)        ; => #t
    #> TRUE
    arl> (rational? 3.14)      ; => #t
    #> TRUE
    arl> (rational? Inf)       ; => #f (infinities are real but not rational)
    #> FALSE
    arl> (rational? NaN)       ; => #f
    #> FALSE

**Note:** In R, all finite floating-point numbers can be represented as
rationals (IEEE 754). R does not have a built-in rational or decimal
type like some Schemes.

**See also:** [real?](#real-p), [integer?](#integer-p),
[number?](#number-p)

------------------------------------------------------------------------

### exact?

Return \#t if x is an exact number (integer storage type in R).

**Signature:** `(exact? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (exact? 5L)           ; => #t (integer type)
    #> TRUE
    arl> (exact? 5.0)          ; => #f (double type)
    #> FALSE
    arl> (exact? (->integer 5)); => #t
    #> TRUE

**Note:** In Scheme, exactness is a property of the number. In R (and
Arl), exactness corresponds to integer storage type. All integers are
exact; all doubles and complex numbers are inexact.

**See also:** [inexact?](#inexact-p),
[exact-\>inexact](#exact-to-inexact) (in `conversions` module)

------------------------------------------------------------------------

### inexact?

Return \#t if x is an inexact number (double or complex in R).

**Signature:** `(inexact? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (inexact? 5.0)        ; => #t
    #> TRUE
    arl> (inexact? (make-rectangular 3 4))  ; => #t
    #> TRUE
    arl> (inexact? 5L)         ; => #f
    #> FALSE

**See also:** [exact?](#exact-p), [inexact-\>exact](#inexact-to-exact)
(in `conversions` module)

------------------------------------------------------------------------

### integer?

Return \#t if x is an integer-valued number.

**Signature:** `(integer? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (integer? 42)         ; => #t
    #> TRUE
    arl> (integer? 42.0)       ; => #t (value is integer even if storage is double)
    #> TRUE
    arl> (integer? 3.14)       ; => #f
    #> FALSE
    arl> (integer? Inf)        ; => #f
    #> FALSE

**Note:** Tests for integer VALUE, not storage type. Use `exact?` to
test storage type.

**See also:** [natural?](#natural-p), [exact?](#exact-p),
[rational?](#rational-p)

------------------------------------------------------------------------

### natural?

Return \#t if x is a natural number (integer \>= 0).

**Signature:** `(natural? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (natural? 0)          ; => #t
    #> TRUE
    arl> (natural? 42)         ; => #t
    #> TRUE
    arl> (natural? -5)         ; => #f
    #> FALSE
    arl> (natural? 3.14)       ; => #f
    #> FALSE

**See also:** [integer?](#integer-p), [positive?](#positive-p),
[non-negative?](#non-negative-p)

------------------------------------------------------------------------

### finite?

Return \#t if x is finite.

**Signature:** `(finite? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (finite? 42)          ; => #t
    #> TRUE
    arl> (finite? Inf)         ; => #f
    #> FALSE
    arl> (finite? NaN)         ; => #f
    #> FALSE
    arl> (finite? -Inf)        ; => #f
    #> FALSE

**See also:** [infinite?](#infinite-p), [nan?](#nan-p),
[rational?](#rational-p)

------------------------------------------------------------------------

### infinite?

Return \#t if x is infinite.

**Signature:** `(infinite? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (infinite? Inf)       ; => #t
    #> TRUE
    arl> (infinite? -Inf)      ; => #t
    #> TRUE
    arl> (infinite? 42)        ; => #f
    #> FALSE
    arl> (infinite? NaN)       ; => #f
    #> FALSE

**See also:** [finite?](#finite-p), [nan?](#nan-p)

------------------------------------------------------------------------

### nan?

Return \#t if x is NaN.

**Signature:** `(nan? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (nan? NaN)            ; => #t
    #> TRUE
    arl> (nan? (/ 0 0))        ; => #t (0/0 = NaN)
    #> TRUE
    arl> (nan? 42)             ; => #f
    #> FALSE
    arl> (nan? Inf)            ; => #f
    #> FALSE

**See also:** [finite?](#finite-p), [infinite?](#infinite-p)

------------------------------------------------------------------------

## Value Predicates

### even?

Return \#t if x is an even number.

**Signature:** `(even? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (even? 4)             ; => #t
    #> TRUE
    arl> (even? 3)             ; => #f
    #> FALSE
    arl> (even? 0)             ; => #t
    #> TRUE

**See also:** [odd?](#odd-p)

------------------------------------------------------------------------

### odd?

Return \#t if x is an odd number.

**Signature:** `(odd? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (odd? 3)              ; => #t
    #> TRUE
    arl> (odd? 4)              ; => #f
    #> FALSE

**See also:** [even?](#even-p)

------------------------------------------------------------------------

### zero?

Return \#t if x is zero.

**Signature:** `(zero? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (zero? 0)             ; => #t
    #> TRUE
    arl> (zero? 0.0)           ; => #t
    #> TRUE
    arl> (zero? 1)             ; => #f
    #> FALSE

**See also:** [positive?](#positive-p), [negative?](#negative-p)

------------------------------------------------------------------------

### positive?

Return \#t if x is greater than zero.

**Signature:** `(positive? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (positive? 5)         ; => #t
    #> TRUE
    arl> (positive? 0)         ; => #f
    #> FALSE
    arl> (positive? -5)        ; => #f
    #> FALSE

**See also:** [negative?](#negative-p),
[non-negative?](#non-negative-p), [zero?](#zero-p)

------------------------------------------------------------------------

### negative?

Return \#t if x is less than zero.

**Signature:** `(negative? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (negative? -5)        ; => #t
    #> TRUE
    arl> (negative? 0)         ; => #f
    #> FALSE
    arl> (negative? 5)         ; => #f
    #> FALSE

**See also:** [positive?](#positive-p),
[non-positive?](#non-positive-p), [zero?](#zero-p)

------------------------------------------------------------------------

### non-negative?

Return \#t if x is greater than or equal to zero.

**Signature:** `(non-negative? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (non-negative? 5)     ; => #t
    #> TRUE
    arl> (non-negative? 0)     ; => #t
    #> TRUE
    arl> (non-negative? -5)    ; => #f
    #> FALSE

**See also:** [positive?](#positive-p), [natural?](#natural-p)

------------------------------------------------------------------------

### non-positive?

Return \#t if x is less than or equal to zero.

**Signature:** `(non-positive? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (non-positive? -5)    ; => #t
    #> TRUE
    arl> (non-positive? 0)     ; => #t
    #> TRUE
    arl> (non-positive? 5)     ; => #f
    #> FALSE

**See also:** [negative?](#negative-p)

------------------------------------------------------------------------

## Equality Predicates

Arl provides `equal?` for deep structural equality with S3-style
dispatch, and `identical?` for R’s native identity comparison. The
Scheme predicates `eq?` and `eqv?` are intentionally not implemented
because R does not provide the pointer-level semantics they require.

### equal?

Deep structural equality. Dispatches on class of first argument.
Optional :strict \#t uses identical? for atomics. Add methods with
(set-method! ’equal? ’my-class (lambda (a b strict) …)).

**Signature:** `(equal? a b [strict #f])`

**Parameters:** - **`a`** — First value - **`b`** — Second value -
**`strict`** — When \#t, use identical? for atomics (default \#f)

**Examples:**

    arl> (equal? 1 1)                    ; => #t
    #> TRUE
    arl> (equal? 1 1.0)                  ; => #t (type coercion)
    #> TRUE
    arl> (equal? 1L 1.0 :strict #t)     ; => #f (strict mode, uses identical?)
    #> FALSE
    arl> (equal? (list 1 2) (list 1 2))  ; => #t (deep structural)
    #> TRUE
    arl> (equal? "hello" "hello")        ; => #t
    #> TRUE

**See also:** [identical?](#identical-p), [eq?](#eq-p), [eqv?](#eqv-p),
[set-method!](#set-method-bang)

------------------------------------------------------------------------

### identical?

R’s native equality test. Structural comparison for value types, pointer
comparison for reference types.

**Signature:** `(identical? a b)`

**Parameters:** - **`a`** — First value - **`b`** — Second value

**Examples:**

    arl> (identical? 1 1)          ; => #t
    #> TRUE
    arl> (identical? 1L 1.0)       ; => #f (integer vs double)
    #> FALSE
    arl> (identical? "a" "a")      ; => #t
    #> TRUE

**Note:** This is R’s
[`identical()`](https://rdrr.io/r/base/identical.html) with no
modifications. Use `equal?` for deep structural equality with type
coercion.

**See also:** [equal?](#equal-p), [eq?](#eq-p), [eqv?](#eqv-p)

------------------------------------------------------------------------

### eq?

**Not implemented in Arl.** Raises an error when called.

**Signature:** `(eq? a b)`

**Parameters:** - **`a`** — First value - **`b`** — Second value

**Note:** True Scheme `eq?` semantics cannot be properly implemented in
R because R does not provide reliable pointer equality for all object
types. R’s [`identical()`](https://rdrr.io/r/base/identical.html) does
structural comparison for some types (lists, vectors) but pointer
comparison for others (environments, reference classes). Use
`identical?` for R’s native equality, or `equal?` for deep structural
equality.

**See also:** [identical?](#identical-p), [equal?](#equal-p)

------------------------------------------------------------------------

### eqv?

**Not implemented in Arl.** Raises an error when called.

**Signature:** `(eqv? a b)`

**Parameters:** - **`a`** — First value - **`b`** — Second value

**Note:** True Scheme `eqv?` semantics cannot be properly implemented in
R because R does not provide reliable pointer equality for all object
types. Use `identical?` for R’s native equality, or `equal?` for deep
structural equality.

**See also:** [identical?](#identical-p), [equal?](#equal-p)

------------------------------------------------------------------------

## Helper Functions for Equal?

Internal helper functions used by the `equal?` dispatch methods to
perform recursive structural comparison of environments and lists.

### env-equal?

Compare two environments by their bindings and values.

**Signature:** `(env-equal? env1 env2)`

**Parameters:** - **`env1`** — First environment - **`env2`** — Second
environment

**Examples:**

    arl> (define e1 (new.env :parent (emptyenv)))
    #> <environment>
    arl> (define e2 (new.env :parent (emptyenv)))
    #> <environment>
    arl> (assign "x" 1 :envir e1)
    #> 1
    arl> (assign "x" 1 :envir e2)
    #> 1
    arl> (env-equal? e1 e2)              ; => #t
    #> TRUE

**Note:** Used internally by `equal?.environment`. Compares environments
by sorting their bindings and recursively comparing values.

**See also:** [equal?.environment](#equal-p-environment)

------------------------------------------------------------------------

### list-equal?

Recursively compare list elements.

**Signature:** `(list-equal? lst1 lst2)`

**Parameters:** - **`lst1`** — First list - **`lst2`** — Second list

**Examples:**

    arl> (list-equal? (list 1 2 3) (list 1 2 3))  ; => #t
    #> TRUE
    arl> (list-equal? (list 1 2) (list 1 2 3))     ; => #f
    #> FALSE
    arl> (list-equal? (list 1 "a") (list 1 "a"))   ; => #t
    #> TRUE

**Note:** Used internally by `equal?.list`. Recursively compares list
elements using `equal?`.

**See also:** [equal?.list](#equal-p-list)

------------------------------------------------------------------------

## S3 Dispatch System

Arl implements a simplified S3-style dispatch system for generic
functions like `equal?`. Types are identified by their first S3 class,
and methods are registered as `generic.class` bindings in the top-level
environment.

### s3-type

Extract the first S3 class from an object.

**Signature:** `(s3-type obj)`

**Parameters:** - **`obj`** — Object to get the S3 class of

**Examples:**

    arl> (s3-type 42)                    ; => "numeric"
    #> "numeric"
    arl> (s3-type "hello")               ; => "character"
    #> "character"
    arl> (s3-type (list 1 2))            ; => "list"
    #> "list"
    arl> (s3-type (new.env))             ; => "environment"
    #> "environment"

------------------------------------------------------------------------

### check-s3-type-match

Check if all objects have the same S3 type.

**Signature:** `(check-s3-type-match obj rest...)`

**Parameters:** - **`obj`** — First object - **`rest`** — Additional
objects to compare against

**Examples:**

    arl> (check-s3-type-match 1 2 3)     ; => #t (all numeric)
    #> TRUE
    arl> (check-s3-type-match 1 "a")     ; => #f (numeric vs character)
    #> FALSE

**Note:** Internal function used by `equal?` to verify type consistency
before dispatch.

------------------------------------------------------------------------

### set-method!

Register an S3-style method. Example: (set-method! ’equal? ’my-class
(lambda (a b strict) …)).

**Signature:** `(set-method! generic-name class-name method-fun)`

**Parameters:** - **`generic-name`** — Symbol naming the generic
function (e.g. ’equal?) - **`class-name`** — Symbol naming the S3 class
to dispatch on - **`method-fun`** — Implementation function for this
class

**Examples:**

    arl> ;; Register a custom equality method for "point" objects:
    arl> (set-method! 'equal? 'point
    arl>   (lambda (a b strict)
    arl>     (and (equal? ($ a "x") ($ b "x"))
    arl>          (equal? ($ a "y") ($ b "y")))))
    #> <function>

**See also:** [use-method](#use-method)

------------------------------------------------------------------------

### use-method

Dispatch to an S3 method based on object class.

**Signature:** `(use-method generic-name obj args)`

**Parameters:** - **`generic-name`** — String naming the generic
(e.g. “equal?”) - **`obj`** — Object whose class determines which method
to call - **`args`** — List of arguments to pass to the method

**Examples:**

    arl> (use-method "equal?" (list 1 2) (list (list 1 2) (list 1 2) #f))  ; dispatches to equal?.list
    #> TRUE

**Note:** Internal function. Dispatches to the appropriate S3 method
based on the object’s class, falling back to the `.default` method if no
class-specific method is found.

**See also:** [set-method!](#set-method-bang), [s3-type](#s3-type)

------------------------------------------------------------------------

## Built-in Equal? Methods

These methods handle equality comparison for R’s core types. The
dispatch system selects the appropriate method based on the S3 class of
the first argument. Users can register additional methods with
`set-method!`.

### equal?.default

Default equality: atomic/vector comparison. :strict \#t =\> \#f; else
use == with type coercion.

**Signature:** `(equal?.default a b [strict #f])`

**Parameters:** - **`a`** — First value - **`b`** — Second value -
**`strict`** — When \#t, use identical? instead of == (default \#f)

**Examples:**

    arl> (equal?.default 1 1)             ; => #t
    #> TRUE
    arl> (equal?.default 1 1.0)           ; => #t (coercion via ==)
    #> TRUE
    arl> (equal?.default 1L 1.0 :strict #t) ; => #f (strict uses identical?)
    #> FALSE
    arl> (equal?.default (c 1 2 3) (c 1 2 3))  ; => #t (element-wise)
    #> TRUE

------------------------------------------------------------------------

### equal?.list

Compare lists recursively by structure and elements.

**Signature:** `(equal?.list a b [strict #f])`

**Parameters:** - **`a`** — First list - **`b`** — Second list -
**`strict`** — When \#t, use strict comparison (default \#f)

**Examples:**

    arl> (equal?.list (list 1 2 3) (list 1 2 3))          ; => #t
    #> TRUE
    arl> (equal?.list (list 1 (list 2 3)) (list 1 (list 2 3)))  ; => #t (nested)
    #> TRUE
    arl> (equal?.list (list 1 2) (list 1 2 3))             ; => #f
    #> FALSE

------------------------------------------------------------------------

### equal?.environment

Compare environments by bindings and values (dict, set, R6, refclass are
env-based).

**Signature:** `(equal?.environment a b [strict #f])`

**Parameters:** - **`a`** — First environment - **`b`** — Second
environment - **`strict`** — When \#t, use strict comparison (default
\#f)

**Examples:**

    arl> (define e1 (new.env :parent (emptyenv)))
    #> <environment>
    arl> (define e2 (new.env :parent (emptyenv)))
    #> <environment>
    arl> (assign "x" 1 :envir e1)
    #> 1
    arl> (assign "x" 1 :envir e2)
    #> 1
    arl> (equal?.environment e1 e2)       ; => #t
    #> TRUE

------------------------------------------------------------------------

## Symbol Conversions

### symbol-\>string

Convert symbol to string.

**Signature:** `(symbol->string sym)`

**Parameters:** - **`sym`** — Symbol to convert to string

**Examples:**

    arl> (symbol->string 'hello)  ; => "hello"
    #> "hello"
    arl> (symbol->string 'x)      ; => "x"
    #> "x"

**Note:** Signals an error if the argument is not a symbol.

**See also:** [string-\>symbol](#string-to-symbol),
[-\>symbol](#to-symbol)

------------------------------------------------------------------------

### string-\>symbol

Convert string to symbol.

**Signature:** `(string->symbol str)`

**Parameters:** - **`str`** — String to convert to symbol

**Examples:**

    arl> (string->symbol "hello")  ; => hello (a symbol)
    #> hello
    arl> (string->symbol "x")      ; => x
    #> x

**Note:** Signals an error if the argument is not a string.

**See also:** [symbol-\>string](#symbol-to-string),
[-\>symbol](#to-symbol)

------------------------------------------------------------------------

### -\>symbol

Convert value to symbol.

**Signature:** `(->symbol x)`

**Parameters:** - **`x`** — Value to convert to symbol

**Examples:**

    arl> (->symbol "hello")  ; => hello (a symbol)
    #> hello
    arl> (->symbol 42)       ; => 42 (symbol named "42")
    #> 42
    arl> (->symbol 'x)       ; => x (already a symbol, returned as-is)
    #> x

**See also:** [symbol-\>string](#symbol-to-string),
[string-\>symbol](#string-to-symbol)

------------------------------------------------------------------------

## Numeric Conversions

### -\>number

Convert value to number.

**Signature:** `(->number x)`

**Parameters:** - **`x`** — Value to convert to number

**Examples:**

    arl> (->number "42")     ; => 42
    #> 42
    arl> (->number "3.14")   ; => 3.14
    #> 3.14
    arl> (->number 5)        ; => 5 (already a number)
    #> 5

**Note:** Signals an error for non-numeric strings
(e.g. `(->number "abc")`).

**See also:** [-\>integer](#to-integer), [-\>double](#to-double),
[-\>complex](#to-complex)

------------------------------------------------------------------------

## Scheme-Style Exact/Inexact Conversions

### exact-\>inexact

Convert exact number to inexact (integer to double).

**Signature:** `(exact->inexact x)`

**Parameters:** - **`x`** — Exact (integer) number to convert to double

**Examples:**

    arl> (exact->inexact 5L)     ; => 5.0 (integer to double)
    #> 5
    arl> (exact->inexact 3.14)   ; => 3.14 (already double, returned as double)
    #> 3.14

**Note:** In Scheme, exactness is a property of any number. In R (and
Arl), exact means integer storage type and inexact means double or
complex. See also `exact?` and `inexact?` in the `math` module.

**See also:** [inexact-\>exact](#inexact-to-exact), [exact?](#exact-p)
(in `types` module), [inexact?](#inexact-p) (in `types` module)

------------------------------------------------------------------------

### inexact-\>exact

Convert inexact number to exact (double to integer, rounds to nearest).

**Signature:** `(inexact->exact x)`

**Parameters:** - **`x`** — Inexact (double) number to round to integer

**Examples:**

    arl> (inexact->exact 3.7)    ; => 4L (rounds to nearest integer)
    #> 4
    arl> (inexact->exact 3.2)    ; => 3L
    #> 3
    arl> (inexact->exact 5L)     ; => 5L (already integer)
    #> 5

**Note:** **Warning:** Converting double to integer rounds to the
nearest integer, which may lose precision. For example,
`(inexact->exact 3.7)` returns `4L`, not `3L`.

**See also:** [exact-\>inexact](#exact-to-inexact), [exact?](#exact-p)
(in `types` module), [inexact?](#inexact-p) (in `types` module)

------------------------------------------------------------------------

## R-Style Numeric Conversions

R-style type conversions providing explicit control over the target
numeric type.

### -\>integer

Convert value to integer.

**Signature:** `(->integer x)`

**Parameters:** - **`x`** — Value to convert to integer

**Examples:**

    arl> (->integer 3.7)     ; => 3L (truncates toward zero)
    #> 3
    arl> (->integer "42")    ; => 42L
    #> 42
    arl> (->integer 5L)      ; => 5L (already integer)
    #> 5

**Note:** Signals an error for non-numeric strings. Note that
`->integer` truncates toward zero (like R’s `as.integer`), whereas
`inexact->exact` rounds to nearest.

**See also:** [-\>number](#to-number), [-\>double](#to-double),
[inexact-\>exact](#inexact-to-exact)

------------------------------------------------------------------------

### -\>double

Convert value to double.

**Signature:** `(->double x)`

**Parameters:** - **`x`** — Value to convert to double

**Examples:**

    arl> (->double 5L)       ; => 5.0
    #> 5
    arl> (->double "3.14")   ; => 3.14
    #> 3.14
    arl> (->double 2.5)      ; => 2.5 (already double)
    #> 2.5

**See also:** [-\>number](#to-number), [-\>integer](#to-integer),
[exact-\>inexact](#exact-to-inexact)

------------------------------------------------------------------------

### -\>complex

Convert value to complex number (imaginary part = 0).

**Signature:** `(->complex x)`

**Parameters:** - **`x`** — Value to convert to complex number

**Examples:**

    arl> (->complex 5)       ; => 5+0i
    #> 5+0i
    arl> (->complex 3.14)    ; => 3.14+0i
    #> 3.14+0i
    arl> (->complex "2+3i")  ; => 2+3i
    #> 2+3i

**See also:** [-\>number](#to-number), [-\>double](#to-double),
[make-rectangular](https://willbrannon.com/arl/articles/lang-math.html#make-rectangular)
(in `types` module)

------------------------------------------------------------------------

## Collection Conversions

### -\>list

Convert value to list.

**Signature:** `(->list x)`

**Parameters:** - **`x`** — Value to convert to list

**Examples:**

    arl> (->list (c 1 2 3))       ; => (1 2 3)
    #> (1 2 3)
    arl> (->list (list 1 2 3))    ; => (1 2 3) (already a list)
    #> (1 2 3)

**See also:** [-\>vector](#to-vector)

------------------------------------------------------------------------

### -\>vector

Convert value to vector.

**Signature:** `(->vector x)`

**Parameters:** - **`x`** — Value to convert to atomic vector

**Examples:**

    arl> (->vector (list 1 2 3))   ; => [1] 1 2 3 (atomic vector)
    #> 1 2 3
    arl> (->vector (c 1 2 3))  ; => [1] 1 2 3 (already a vector)
    #> 1 2 3

**Note:** When given a list, `->vector` flattens it into an atomic
vector using `unlist`. Nested lists will be recursively flattened.

**See also:** [-\>list](#to-list)

------------------------------------------------------------------------
