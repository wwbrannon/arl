# Standard Library: Higher-Order Functions

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## Core Higher-Order Functions

The fundamental building blocks for functional programming in Arl. These
functions operate on lists and follow the convention of taking the
function argument first, then the data argument.

### map

Apply function to each element. With one list, maps fn over it. With
multiple lists, maps fn over corresponding elements (like Scheme’s map).

**Signature:** `(map fn lst rest...)`

**Parameters:** - **`fn`** — Function to apply to each element (or
corresponding elements) - **`lst`** — First list to transform -
**`rest`** — Additional lists for multi-list mapping

**Examples:**

    arl> (map (lambda (x) (* x 2)) (list 1 2 3))  ; => (2 4 6)
    #> (2 4 6)
    arl> (map + (list 1 2 3) (list 4 5 6))         ; => (5 7 9)
    #> (5 7 9)

**See also:** [filter](#filter), [mapcat](#mapcat), [reduce](#reduce)

------------------------------------------------------------------------

### mapcat

Map then concatenate results. Uses do.call(c, …) for O(n) concatenation.

**Signature:** `(mapcat fn lst)`

**Parameters:** - **`fn`** — Function that returns a list for each
element - **`lst`** — List to transform

**Examples:**

    arl> (mapcat (lambda (x) (list x (* x 10))) (list 1 2 3))  ; => (1 10 2 20 3 30)
    #> (1 10 2 20 3 30)
    arl> (mapcat (lambda (x) (list x x)) (list 1 2 3))          ; => (1 1 2 2 3 3)
    #> (1 1 2 2 3 3)

**See also:** [map](#map),
[flatten](https://willbrannon.com/arl/articles/lang-list-seq.html#flatten)

------------------------------------------------------------------------

### filter

Filter items by predicate.

**Signature:** `(filter pred lst)`

**Parameters:** - **`pred`** — Predicate function; elements where it
returns \#t are kept - **`lst`** — List to filter

**Examples:**

    arl> (filter (lambda (x) (> x 2)) (list 1 2 3 4 5))  ; => (3 4 5)
    #> (3 4 5)
    arl> (filter even? (list 1 2 3 4 5 6))                 ; => (2 4 6)
    #> (2 4 6)
    arl> (filter string? (list 1 "a" 2 "b"))               ; => ("a" "b")
    #> ("a" "b")

**See also:** [remove](#remove),
[take-while](https://willbrannon.com/arl/articles/lang-list-seq.html#take-while),
[drop-while](https://willbrannon.com/arl/articles/lang-list-seq.html#drop-while)

------------------------------------------------------------------------

### remove

Remove items where predicate is true.

**Signature:** `(remove pred lst)`

**Parameters:** - **`pred`** — Predicate function; elements where it
returns \#t are removed - **`lst`** — List to filter

**Examples:**

    arl> (remove even? (list 1 2 3 4 5 6))                 ; => (1 3 5)
    #> (1 3 5)
    arl> (remove (lambda (x) (> x 3)) (list 1 2 3 4 5))   ; => (1 2 3)
    #> (1 2 3)

**See also:** [filter](#filter)

------------------------------------------------------------------------

### reduce

Reduce list with function.

**Signature:** `(reduce fn lst rest...)`

**Parameters:** - **`fn`** — Binary function taking accumulator and
current element - **`lst`** — List to reduce - **`rest`** — Optional
initial accumulator value

**Examples:**

    arl> (reduce + (list 1 2 3 4))       ; => 10
    #> 10
    arl> (reduce * (list 1 2 3 4))       ; => 24
    #> 24
    arl> (reduce + (list 1 2 3) 100)     ; => 106 (with initial value)
    #> 106
    arl> (reduce string-append (list "a" "b" "c"))  ; => "abc"
    #> "abc"

**See also:** [foldl](#foldl), [foldr](#foldr), [map](#map)

------------------------------------------------------------------------

### foldl

Left fold alias for reduce. Applies fn as (fn acc elem).

**Signature:** `(foldl fn lst rest...)`

**Parameters:** - **`fn`** — Binary function taking accumulator and
current element - **`lst`** — List to fold over - **`rest`** — Optional
initial accumulator value

**Examples:**

    arl> (foldl + (list 1 2 3 4))        ; => 10
    #> 10
    arl> (foldl - (list 1 2 3) 10)       ; => 4 (10 - 1 - 2 - 3)
    #> 4
    arl> (foldl (lambda (acc x) (cons x acc)) (list 1 2 3) (list))  ; => (3 2 1)
    #> (3 2 1)

**See also:** [foldr](#foldr), [reduce](#reduce)

------------------------------------------------------------------------

### foldr

Right fold. Applies fn as (fn elem acc).

**Signature:** `(foldr fn lst rest...)`

**Parameters:** - **`fn`** — Binary function taking current element and
accumulator - **`lst`** — List to fold over from the right - **`rest`**
— Optional initial accumulator value

**Examples:**

    arl> (foldr + (list 1 2 3 4))        ; => 10
    #> 10
    arl> (foldr - (list 1 2 3) 10)       ; => -8 (1 - (2 - (3 - 10)))
    #> -8
    arl> (foldr cons (list 1 2 3) (list)) ; => (1 2 3) (preserves order)
    #> (1 2 3)

**See also:** [foldl](#foldl), [reduce](#reduce)

------------------------------------------------------------------------

### every?

Return \#t if predicate true for all items.

**Signature:** `(every? pred lst)`

**Parameters:** - **`pred`** — Predicate function to test each element -
**`lst`** — List to check

**Examples:**

    arl> (every? positive? (list 1 2 3))       ; => #t
    #> TRUE
    arl> (every? even? (list 2 4 6))           ; => #t
    #> TRUE
    arl> (every? even? (list 2 3 6))           ; => #f
    #> FALSE
    arl> (every? string? (list "a" "b" "c"))   ; => #t
    #> TRUE

**See also:** [any?](#any-p), [filter](#filter)

------------------------------------------------------------------------

### any?

Return \#t if predicate true for any item.

**Signature:** `(any? pred lst)`

**Parameters:** - **`pred`** — Predicate function to test each element -
**`lst`** — List to check

**Examples:**

    arl> (any? even? (list 1 3 4 5))           ; => #t
    #> TRUE
    arl> (any? negative? (list 1 2 3))         ; => #f
    #> FALSE
    arl> (any? string? (list 1 "a" 2))         ; => #t
    #> TRUE

**See also:** [every?](#every-p), [filter](#filter)

------------------------------------------------------------------------

### complement

Negate predicate.

**Signature:** `(complement pred)`

**Parameters:** - **`pred`** — Predicate function to negate

**Examples:**

    arl> (define not-even? (complement even?))
    #> <function>
    arl> (not-even? 3)                          ; => #t
    #> TRUE
    arl> (not-even? 4)                          ; => #f
    #> FALSE
    arl> (filter (complement null?) (list 1 #nil 2 #nil 3))  ; => (1 2 3)
    #> (1 2 3)

**See also:**
[not](https://willbrannon.com/arl/articles/lang-core.html#not),
[filter](#filter)

------------------------------------------------------------------------

### compose

Compose two functions.

**Signature:** `(compose f g)`

**Parameters:** - **`f`** — Outer function (applied second) - **`g`** —
Inner function (applied first)

**Examples:**

    arl> (define add1-then-double (compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))))
    #> <function>
    arl> (add1-then-double 3)                   ; => 8  ((3+1)*2)
    #> 8
    arl> (define abs-sum (compose abs +))
    #> <function>
    arl> (abs-sum -3 -4)                        ; => 7
    #> 7

**See also:** [partial](#partial)

------------------------------------------------------------------------

### partial

Partially apply function.

**Signature:** `(partial fn captured...)`

**Parameters:** - **`fn`** — Function to partially apply -
**`captured`** — Arguments to bind to fn’s first positions

**Examples:**

    arl> (define add5 (partial + 5))
    #> <function>
    arl> (add5 3)                               ; => 8
    #> 8
    arl> (define greet (partial string-append "Hello, "))
    #> <function>
    arl> (greet "world")                        ; => "Hello, world"
    #> "Hello, world"

**See also:** [compose](#compose)

------------------------------------------------------------------------

## Advanced Functional Programming

Higher-order combinators for advanced functional patterns including
currying, juxtaposition, memoization, and iteration.

### curry

Curry a function - enables partial application with optional initial
arguments.

**Signature:** `(curry fn initial-args...)`

**Parameters:** - **`fn`** — Function to curry (must have fixed arity) -
**`initial-args`** — Optional arguments to apply immediately

**Examples:**

    arl> (define add (curry (lambda (a b) (+ a b))))
    #> <function>
    arl> (define add5 (add 5))
    #> <function>
    arl> (add5 3)                               ; => 8
    #> 8
    arl> (define multiply (curry (lambda (a b) (* a b))))
    #> <function>
    arl> ((multiply 3) 4)                       ; => 12
    #> 12

**See also:** [partial](#partial)

------------------------------------------------------------------------

### juxt

Juxtaposition - apply multiple functions to same args, return list of
results.

**Signature:** `(juxt fns...)`

**Parameters:** - **`fns`** — Functions to apply to the arguments

**Examples:**

    arl> (define stats (juxt min max mean))
    #> <function>
    arl> (stats (c 1 2 3 4 5))                  ; => (1 5 3)
    #> (1 5 3)
    arl> (define first-and-last (juxt car last))
    #> <function>
    arl> (first-and-last (list 1 2 3))          ; => (1 3)
    #> (1 3)

**See also:** [map](#map), [compose](#compose)

------------------------------------------------------------------------

### constantly

Return a function that always returns the given value.

**Signature:** `(constantly value)`

**Parameters:** - **`value`** — Value to return regardless of arguments

**Examples:**

    arl> (define always-42 (constantly 42))
    #> <function>
    arl> (always-42)                             ; => 42
    #> 42
    arl> (always-42 "ignored" "args")            ; => 42
    #> 42
    arl> (map (constantly 0) (list 1 2 3))       ; => (0 0 0)
    #> (0 0 0)

------------------------------------------------------------------------

### iterate

Apply function n times to initial value.

**Signature:** `(iterate fn n init)`

**Parameters:** - **`fn`** — Unary function to apply repeatedly -
**`n`** — Number of times to apply fn - **`init`** — Starting value

**Examples:**

    arl> (iterate (lambda (x) (* x 2)) 5 1)     ; => 32  (1 -> 2 -> 4 -> 8 -> 16 -> 32)
    #> 32
    arl> (iterate inc 3 0)                       ; => 3   (0 -> 1 -> 2 -> 3)
    #> 3
    arl> (iterate (lambda (x) (* x x)) 2 2)     ; => 16  (2 -> 4 -> 16)
    #> 16

**See also:** [iterate-until](#iterate-until), [reduce](#reduce)

------------------------------------------------------------------------

### iterate-until

Apply function starting from init, collecting values until predicate
becomes true. Includes all values where predicate is false, stops when
next value would satisfy predicate.

**Signature:** `(iterate-until fn init pred)`

**Parameters:** - **`fn`** — Unary function to apply repeatedly -
**`init`** — Starting value - **`pred`** — Predicate that signals when
to stop collecting

**Examples:**

    arl> (iterate-until (lambda (x) (* x 2)) 1 (lambda (x) (> x 100)))
    #> (1 2 4 8 16 32 64)
    arl>   ; => (1 2 4 8 16 32 64)  (stops before 128 which exceeds 100)
    arl> (iterate-until inc 0 (lambda (x) (>= x 5)))
    #> (0 1 2 3 4)
    arl>   ; => (0 1 2 3 4)

**See also:** [iterate](#iterate),
[take-while](https://willbrannon.com/arl/articles/lang-list-seq.html#take-while)

------------------------------------------------------------------------

### memoize

Memoize function - cache results for previously seen arguments.

**Signature:** `(memoize fn)`

**Parameters:** - **`fn`** — Function whose results to cache by
arguments

**Examples:**

    arl> (define slow-fib (lambda (n) (if (< n 2) n (+ (slow-fib (- n 1)) (slow-fib (- n 2))))))
    #> <function>
    arl> (define fast-fib (memoize slow-fib))
    #> <function>
    arl> (fast-fib 10)                          ; => 55
    #> 55
    arl> (fast-fib 10)                          ; => 55 (cached, instant)
    #> 55

------------------------------------------------------------------------

## Iteration and Counting

Functions for side-effecting iteration and counting over sequences.

### for-each

Apply function to each element for side effects. Returns \#nil. Supports
multiple lists like multi-list map.

**Signature:** `(for-each fn lst rest...)`

**Parameters:** - **`fn`** — Function to apply for side effects -
**`lst`** — First list - **`rest`** — Additional lists for multi-list
iteration

**Examples:**

    arl> (for-each print '(1 2 3))  ; prints 1 2 3, returns #nil
    #> [1] 1
    #> [1] 2
    #> [1] 3

**See also:** [map](#map)

------------------------------------------------------------------------

### count

Count elements matching predicate.

**Signature:** `(count pred lst)`

**Parameters:** - **`pred`** — Predicate function to test each element -
**`lst`** — List to count matching elements in

**Examples:**

    arl> (count even? (list 1 2 3 4))  ; => 2
    #> 2
    arl> (count even? (list))           ; => 0
    #> 0

**See also:** [filter](#filter), [every?](#every-p), [any?](#any-p)

------------------------------------------------------------------------

### map-indexed

Map with index. Calls (fn index element) for each element, with 0-based
indices.

**Signature:** `(map-indexed fn lst)`

**Parameters:** - **`fn`** — Function taking index and element -
**`lst`** — List to map over

**Examples:**

    arl> (map-indexed list '(a b c))  ; => ((0 a) (1 b) (2 c))
    #> ((0 a) (1 b) (2 c))

**See also:** [map](#map)

------------------------------------------------------------------------

### group-by

Group elements by key function. Returns a dict mapping each key (as
string) to a list of matching elements.

**Signature:** `(group-by key-fn lst)`

**Parameters:** - **`key-fn`** — Function to compute the grouping key
for each element - **`lst`** — List to group

**Examples:**

    arl> (group-by even? '(1 2 3 4))  ; => dict with TRUE->(2 4), FALSE->(1 3)
    #> (1 3) (2 4)

**See also:** [frequencies](#frequencies), [filter](#filter)

------------------------------------------------------------------------

### frequencies

Count occurrences of each element. Returns a dict mapping each element
(as string) to its count.

**Signature:** `(frequencies lst)`

**Parameters:** - **`lst`** — List to count element frequencies in

**Examples:**

    arl> (frequencies '(a b a c b a))  ; => dict with a->3 b->2 c->1
    #> 3 2 1

**See also:** [group-by](#group-by), [count](#count)

------------------------------------------------------------------------

## Logical Operations

Core logical operations for Arl. Note that `and` and `or` are compiler
special forms (not functions) with short-circuit evaluation, so they are
not part of this module.

### not

Logical negation with Arl truthiness.

**Signature:** `(not x)`

**Parameters:** - **`x`** — Value to negate

**Examples:**

    arl> (not #t)          ; => #f
    #> FALSE
    arl> (not #f)          ; => #t
    #> TRUE
    arl> (not 0)           ; => #t (0 is falsy)
    #> TRUE
    arl> (not 1)           ; => #f (non-zero is truthy)
    #> FALSE
    arl> (not #nil)        ; => #t (NULL is falsy)
    #> TRUE
    arl> (not "hello")     ; => #f (non-empty values are truthy)
    #> FALSE

**Note:** Arl follows Scheme-like truthiness: \#f/FALSE, \#nil/NULL, and
0 are falsy. Everything else is truthy. This differs from R where only
FALSE and NULL are falsy.

**See also:** [xor](#xor),
[and](https://willbrannon.com/arl/articles/lang-core.html#and),
[or](https://willbrannon.com/arl/articles/lang-core.html#or) (special
forms)

------------------------------------------------------------------------

### xor

Logical exclusive OR with Arl truthiness.

**Signature:** `(xor a b)`

**Parameters:** - **`a`** — First value - **`b`** — Second value

**Examples:**

    arl> (xor #t #f)       ; => #t
    #> TRUE
    arl> (xor #f #t)       ; => #t
    #> TRUE
    arl> (xor #t #t)       ; => #f
    #> FALSE
    arl> (xor #f #f)       ; => #f
    #> FALSE
    arl> (xor 1 0)         ; => #t (1 is truthy, 0 is falsy)
    #> TRUE
    arl> (xor 1 2)         ; => #f (both truthy)
    #> FALSE

**Note:** Exclusive OR: returns \#t when exactly one argument is truthy,
\#f when both are truthy or both are falsy.

**See also:**
[not](https://willbrannon.com/arl/articles/lang-core.html#not),
[and](https://willbrannon.com/arl/articles/lang-core.html#and),
[or](https://willbrannon.com/arl/articles/lang-core.html#or) (special
forms)

------------------------------------------------------------------------
