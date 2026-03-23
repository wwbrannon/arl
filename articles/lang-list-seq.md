# Standard Library: Lists and Sequences

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## List Operations

Core list constructors and accessors, following Scheme conventions.
Lists in Arl are backed by R lists; [`car`](#car)/[`cdr`](#cdr) provide
the fundamental decomposition and [`cons`](#cons) the fundamental
construction.

### call

Convert a list to a callable form.

**Signature:** `(call lst)`

**Parameters:** - **`lst`** — List to convert to a call object.

**Examples:**

    arl> (call (list '+ 1 2))  ; => language: (+ 1 2)
    #> (+ 1 2)

**See also:**
[eval](https://willbrannon.com/arl/articles/lang-core.html#eval), apply

------------------------------------------------------------------------

### caar

car of car.

**Signature:** `(caar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cadr

car of cdr.

**Signature:** `(cadr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### cdar

cdr of car.

**Signature:** `(cdar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cddr

cdr of cdr.

**Signature:** `(cddr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### caaar

car of car of car.

**Signature:** `(caaar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### caadr

car of car of cdr.

**Signature:** `(caadr x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cadar

car of cdr of car.

**Signature:** `(cadar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### caddr

car of cdr of cdr.

**Signature:** `(caddr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### cdaar

cdr of car of car.

**Signature:** `(cdaar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cdadr

cdr of car of cdr.

**Signature:** `(cdadr x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cddar

cdr of cdr of car.

**Signature:** `(cddar x)`

**Parameters:** - **`x`** — Nested list to access.

------------------------------------------------------------------------

### cdddr

cdr of cdr of cdr.

**Signature:** `(cdddr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### cadddr

car of cdr of cdr of cdr.

**Signature:** `(cadddr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### cddddr

cdr of cdr of cdr of cdr.

**Signature:** `(cddddr x)`

**Parameters:** - **`x`** — List to access.

------------------------------------------------------------------------

### list\*

Build list ending with last arg.

**Signature:** `(list* args...)`

**Parameters:** - **`args`** — Elements to combine; the last argument is
appended as a tail.

**Examples:**

    arl> (list* 1 2 (list 3 4))  ; => (1 2 3 4)
    #> (1 2 3 4)
    arl> (list* 1 2 3)           ; => (1 2 3)
    #> (1 2 3)
    arl> (list*)                  ; => ()
    #> ()

**See also:** list, [cons](#cons), [append](#append)

------------------------------------------------------------------------

### append

Concatenate multiple lists into one.

**Signature:** `(append lists...)`

**Parameters:** - **`lists`** — Lists to concatenate in order.

**Examples:**

    arl> (append (list 1 2) (list 3 4))    ; => (1 2 3 4)
    #> (1 2 3 4)
    arl> (append (list 1) (list 2) (list 3)); => (1 2 3)
    #> (1 2 3)
    arl> (append (list 1 2) (list))         ; => (1 2)
    #> (1 2)

**See also:** [cons](#cons), [list\*](#list-star)

------------------------------------------------------------------------

### reverse

Reverse list.

**Signature:** `(reverse x)`

**Parameters:** - **`x`** — List to reverse.

**Examples:**

    arl> (reverse (list 1 2 3))  ; => (3 2 1)
    #> (3 2 1)
    arl> (reverse (list))        ; => ()
    #> ()

------------------------------------------------------------------------

### first

Alias for car.

**Signature:** `(first lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (first (list 10 20 30))   ; => 10
    #> 10

**See also:** [car](#car), [rest](#rest), [last](#last)

------------------------------------------------------------------------

### second

Return second element or \#nil.

**Signature:** `(second lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (second (list 10 20 30))  ; => 20
    #> 20

**See also:** [first](#first), [third](#third)

------------------------------------------------------------------------

### third

Return third element or \#nil.

**Signature:** `(third lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (third (list 10 20 30))   ; => 30
    #> 30

**See also:** [second](#second), [fourth](#fourth)

------------------------------------------------------------------------

### fourth

Return fourth element or \#nil.

**Signature:** `(fourth lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (fourth (list 1 2 3 4))   ; => 4
    #> 4

**See also:** [third](#third)

------------------------------------------------------------------------

### rest

Alias for cdr.

**Signature:** `(rest lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (rest (list 1 2 3))       ; => (2 3)
    #> (2 3)

**See also:** [cdr](#cdr), [first](#first)

------------------------------------------------------------------------

### last

Return last item or \#nil.

**Signature:** `(last lst)`

**Parameters:** - **`lst`** — Source list.

**Examples:**

    arl> (last (list 1 2 3))   ; => 3
    #> 3
    arl> (last (list 42))      ; => 42
    #> 42
    arl> (last (list))         ; => #nil

**See also:** [first](#first), [nth](#nth)

------------------------------------------------------------------------

### nth

Return nth item (0-based).

**Signature:** `(nth lst n)`

**Parameters:** - **`lst`** — Source list. - **`n`** — 0-based index of
the element to retrieve.

**Examples:**

    arl> (nth (list 'a 'b 'c) 0)  ; => a
    #> a
    arl> (nth (list 'a 'b 'c) 2)  ; => c
    #> c

**See also:** [first](#first), [last](#last)

------------------------------------------------------------------------

### car

Return first element or \#nil.

**Signature:** `(car lst)`

**Examples:**

    arl> (car (list 1 2 3))    ; => 1
    #> 1
    arl> (car (list))           ; => #nil
    arl> (car (cons 'a 'b))    ; => a
    #> a

**See also:** [cdr](#cdr), [cons](#cons), [first](#first)

------------------------------------------------------------------------

### cdr

Return list without first element (or cdr of dotted pair).

**Signature:** `(cdr lst)`

**Examples:**

    arl> (cdr (list 1 2 3))    ; => (2 3)
    #> (2 3)
    arl> (cdr (list 1))        ; => ()
    #> ()
    arl> (cdr (cons 'a 'b))    ; => b
    #> b

**See also:** [car](#car), [cons](#cons), [rest](#rest)

------------------------------------------------------------------------

### cons

Prepend item to list or call; dotted pair when cdr is not a list.

**Signature:** `(cons item lst)`

**Examples:**

    arl> (cons 1 (list 2 3))   ; => (1 2 3)
    #> (1 2 3)
    arl> (cons 'a (list 'b))   ; => (a b)
    #> (a b)
    arl> (cons 'a 'b)          ; => dotted pair (a . b)
    #> (a . b)

**See also:** [car](#car), [cdr](#cdr), [append](#append)

------------------------------------------------------------------------

## Association Lists

Association lists (alists) map keys to values. Each entry is a list
whose `car` is the key and whose `cadr` is the value. `assoc` and
friends search by key; `rassoc` searches by value.

### assoc

Find first alist entry (list or dotted pair) with matching key (uses
==).

**Signature:** `(assoc key alist)`

**Parameters:** - **`key`** — Key to search for. - **`alist`** —
Association list of (key value) entries.

**Examples:**

    arl> (assoc 'b (list (list 'a 1) (list 'b 2) (list 'c 3)))  ; => (b 2)
    #> (b 2)
    arl> (assoc 'z (list (list 'a 1) (list 'b 2)))               ; => #nil

**See also:** [rassoc](#rassoc),
[assoc-by-identical?](#assoc-by-identical-p),
[assoc-by-==](#assoc-by-eq-eq)

------------------------------------------------------------------------

### assoc-by-equal?

Find first alist entry with key matching under equal? (deep structural
comparison). Requires `(import equality)` for `equal?`.

**Parameters:** - **`key`** — Key to search for. - **`alist`** —
Association list of (key value) entries.

**See also:** [assoc](#assoc)

------------------------------------------------------------------------

### assoc-by-identical?

Find first alist entry with matching key (uses R’s identical?).

**Signature:** `(assoc-by-identical? key alist)`

**Parameters:** - **`key`** — Key to search for. - **`alist`** —
Association list of (key value) entries.

**See also:** [assoc](#assoc), [assoc-by-==](#assoc-by-eq-eq)

------------------------------------------------------------------------

### assoc-by-==

Find first alist entry with matching key (uses R’s ==).

**Signature:** `(assoc-by-== key alist)`

**Parameters:** - **`key`** — Key to search for. - **`alist`** —
Association list of (key value) entries.

**See also:** [assoc](#assoc),
[assoc-by-identical?](#assoc-by-identical-p)

------------------------------------------------------------------------

### assq

assq cannot be properly implemented in R (no eq?). Use assoc (equal?),
assoc-by-identical?, or assoc-by-== instead.

**Signature:** `(assq key alist)`

**Parameters:** - **`key`** — Key to search for (unused; raises
error). - **`alist`** — Association list (unused; raises error).

**Note:** assq cannot be properly implemented in R (no eq?). Use assoc,
assoc-by-identical?, or assoc-by-== instead.

------------------------------------------------------------------------

### assv

assv cannot be properly implemented in R (no eqv?). Use assoc (equal?),
assoc-by-identical?, or assoc-by-== instead.

**Signature:** `(assv key alist)`

**Parameters:** - **`key`** — Key to search for (unused; raises
error). - **`alist`** — Association list (unused; raises error).

**Note:** assv cannot be properly implemented in R (no eqv?). Use assoc,
assoc-by-identical?, or assoc-by-== instead.

------------------------------------------------------------------------

### rassoc

Find first alist entry with matching value (searches cdr of entries;
uses equal?).

**Signature:** `(rassoc value alist)`

**Parameters:** - **`value`** — Value to search for in the second
position of each entry. - **`alist`** — Association list of (key value)
entries.

**Examples:**

    arl> (rassoc 2 (list (list 'a 1) (list 'b 2) (list 'c 3)))  ; => (b 2)
    #> (b 2)
    arl> (rassoc 9 (list (list 'a 1) (list 'b 2)))               ; => #nil

**See also:** [assoc](#assoc), [rassoc-by-equal?](#rassoc-by-equal-p)

------------------------------------------------------------------------

### rassoc-by-equal?

Alias for rassoc: find first alist entry with value matching under
equal?.

**Signature:** `(rassoc-by-equal? value alist)`

**Parameters:** - **`value`** — Value to search for in the second
position of each entry. - **`alist`** — Association list of (key value)
entries.

**See also:** [rassoc](#rassoc)

------------------------------------------------------------------------

## List Generation

Utilities for creating lists of numbers or repeated values.

### range

Generate numeric range from start to end (exclusive) with optional step.

**Signature:** `(range start end step-args...)`

**Parameters:** - **`start`** — Start value (inclusive). - **`end`** —
End value (exclusive). - **`step-args`** — Optional step size (default
1).

**Examples:**

    arl> (range 0 5)       ; => (0 1 2 3 4)
    #> (0 1 2 3 4)
    arl> (range 0 10 2)    ; => (0 2 4 6 8)
    #> (0 2 4 6 8)
    arl> (range 5 0 -1)    ; => (5 4 3 2 1)
    #> (5 4 3 2 1)

**See also:** [iota](#iota), [make-list](#make-list)

------------------------------------------------------------------------

### iota

Generate sequence of count numbers starting from start (default 0) with
step (default 1).

**Signature:** `(iota count args...)`

**Parameters:** - **`count`** — Number of elements to generate. -
**`args`** — Optional start (default 0) and step (default 1).

**Examples:**

    arl> (iota 5)          ; => (0 1 2 3 4)
    #> (0 1 2 3 4)
    arl> (iota 5 1)        ; => (1 2 3 4 5)
    #> (1 2 3 4 5)
    arl> (iota 5 0 2)      ; => (0 2 4 6 8)
    #> (0 2 4 6 8)

**See also:** [range](#range), [make-list](#make-list)

------------------------------------------------------------------------

### make-list

Create list of n copies of value.

**Signature:** `(make-list n value)`

**Parameters:** - **`n`** — Number of copies. - **`value`** — Element to
repeat.

**Examples:**

    arl> (make-list 3 'x)    ; => (x x x)
    #> (x x x)
    arl> (make-list 0 'x)    ; => ()
    #> ()

**See also:** [iota](#iota), [range](#range)

------------------------------------------------------------------------

## Additional List Accessors

### list-ref

Scheme-style list accessor (alias for nth).

**Signature:** `(list-ref lst index)`

**Parameters:** - **`lst`** — Source list. - **`index`** — 0-based index
of the element to retrieve.

**Examples:**

    arl> (list-ref (list 'a 'b 'c) 1)  ; => b
    #> b

**See also:** [nth](#nth)

------------------------------------------------------------------------

### list-tail

Return list without first k elements.

**Signature:** `(list-tail lst k)`

**Parameters:** - **`lst`** — Source list. - **`k`** — Number of leading
elements to skip.

**Examples:**

    arl> (list-tail (list 'a 'b 'c 'd) 2)  ; => (c d)
    #> (c d)
    arl> (list-tail (list 1 2 3) 0)         ; => (1 2 3)
    #> (1 2 3)

**See also:** [drop](#drop)

------------------------------------------------------------------------

## Sequence Helpers

Higher-level operations for slicing, chunking, and transforming
sequences. These complement the core list operations in the `list`
module with lazy-style idioms common in functional programming.

### take

Take first n items.

**Signature:** `(take n lst)`

**Parameters:** - **`n`** — Number of elements to take - **`lst`** —
Source list

**Examples:**

    arl> (take 3 (list 1 2 3 4 5))  ; => (1 2 3)
    #> (1 2 3)
    arl> (take 0 (list 1 2 3))      ; => ()
    #> ()
    arl> (take 10 (list 1 2))       ; => (1 2)
    #> (1 2)

**See also:** [drop](#drop), [take-while](#take-while)

------------------------------------------------------------------------

### drop

Drop first n items.

**Signature:** `(drop n lst)`

**Parameters:** - **`n`** — Number of elements to skip - **`lst`** —
Source list

**Examples:**

    arl> (drop 2 (list 1 2 3 4 5))  ; => (3 4 5)
    #> (3 4 5)
    arl> (drop 0 (list 1 2 3))      ; => (1 2 3)
    #> (1 2 3)
    arl> (drop 10 (list 1 2))       ; => ()
    #> ()

**See also:** [take](#take), [drop-while](#drop-while)

------------------------------------------------------------------------

### take-while

Take items while predicate is true.

**Signature:** `(take-while pred lst)`

**Parameters:** - **`pred`** — Predicate function; taking stops at first
\#f - **`lst`** — Source list

**Examples:**

    arl> (take-while odd? (list 1 3 5 2 4))   ; => (1 3 5)
    #> (1 3 5)
    arl> (take-while even? (list 1 2 3))       ; => ()
    #> ()

**See also:** [take](#take), [drop-while](#drop-while),
[filter](https://willbrannon.com/arl/articles/lang-functional.html#filter)

------------------------------------------------------------------------

### drop-while

Drop items while predicate is true.

**Signature:** `(drop-while pred lst)`

**Parameters:** - **`pred`** — Predicate function; dropping stops at
first \#f - **`lst`** — Source list

**Examples:**

    arl> (drop-while odd? (list 1 3 5 2 4))   ; => (2 4)
    #> (2 4)
    arl> (drop-while even? (list 1 2 3))       ; => (1 2 3)
    #> (1 2 3)

**See also:** [drop](#drop), [take-while](#take-while),
[filter](https://willbrannon.com/arl/articles/lang-functional.html#filter)

------------------------------------------------------------------------

### partition

Split list into chunks of size n.

**Signature:** `(partition n lst [step n])`

**Parameters:** - **`n`** — Chunk size - **`lst`** — Source list -
**`step`** — Offset between chunk starts (default n, i.e. no overlap)

**Examples:**

    arl> (partition 2 (list 1 2 3 4 5 6))       ; => ((1 2) (3 4) (5 6))
    #> ((1 2) (3 4) (5 6))
    arl> (partition 3 (list 1 2 3 4 5 6) 2)     ; => ((1 2 3) (3 4 5))
    #> ((1 2 3) (3 4 5))
    arl> (partition 2 (list 1 2 3 4 5))          ; => ((1 2) (3 4))
    #> ((1 2) (3 4))

**See also:** [take](#take), [drop](#drop)

------------------------------------------------------------------------

### flatten

Flatten nested lists.

**Signature:** `(flatten lst)`

**Parameters:** - **`lst`** — Possibly nested list to flatten into a
single-level list

**Examples:**

    arl> (flatten (list 1 (list 2 3) (list 4 (list 5))))  ; => (1 2 3 4 5)
    #> (1 2 3 4 5)
    arl> (flatten (list 1 2 3))                            ; => (1 2 3)
    #> (1 2 3)

**See also:**
[mapcat](https://willbrannon.com/arl/articles/lang-functional.html#mapcat),
[append](#append)

------------------------------------------------------------------------

### repeatedly

Call fn n times collecting results.

**Signature:** `(repeatedly n fn)`

**Parameters:** - **`n`** — Number of times to call fn - **`fn`** —
Zero-argument function to call repeatedly

**Examples:**

    arl> (repeatedly 3 (lambda () 42))  ; => (42 42 42)
    #> (42 42 42)

**See also:** [repeat](#repeat),
[map](https://willbrannon.com/arl/articles/lang-functional.html#map)

------------------------------------------------------------------------

### repeat

Repeat value n times.

**Signature:** `(repeat n value)`

**Parameters:** - **`n`** — Number of repetitions - **`value`** — Value
to repeat

**Examples:**

    arl> (repeat 4 'x)   ; => (x x x x)
    #> (x x x x)
    arl> (repeat 3 0)     ; => (0 0 0)
    #> (0 0 0)

**See also:** [repeatedly](#repeatedly)

------------------------------------------------------------------------

### zip

Zip lists into list of tuples.

**Signature:** `(zip lists...)`

**Parameters:** - **`lists`** — Two or more lists to interleave
element-wise

**Examples:**

    arl> (zip (list 1 2 3) (list 'a 'b 'c))          ; => ((1 a) (2 b) (3 c))
    #> ((1 a) (2 b) (3 c))
    arl> (zip (list 1 2) (list 'a 'b) (list 'x 'y))  ; => ((1 a x) (2 b y))
    #> ((1 a x) (2 b y))

**See also:**
[map](https://willbrannon.com/arl/articles/lang-functional.html#map)

------------------------------------------------------------------------

### member

Return sublist of lst starting at first element equal to x, or \#f if
not found. Uses equal? by default; pass :use-identical \#t for R’s
identical().

**Signature:** `(member x lst [use-identical #f])`

**Parameters:** - **`x`** — Value to search for - **`lst`** — List to
search in - **`use-identical`** — Use R’s identical() instead of equal?
(default \#f)

**Examples:**

    arl> (member 3 (list 1 2 3 4 5))   ; => (3 4 5)
    #> (3 4 5)
    arl> (member 9 (list 1 2 3))       ; => #f
    #> FALSE

**See also:** [contains?](#contains-p)

------------------------------------------------------------------------

### contains?

Return \#t if lst contains element equal to x.

**Signature:** `(contains? x lst)`

**Parameters:** - **`x`** — Value to search for - **`lst`** — List to
search in

**Examples:**

    arl> (contains? 2 (list 1 2 3))   ; => #t
    #> TRUE
    arl> (contains? 9 (list 1 2 3))   ; => #f
    #> FALSE

**See also:** [member](#member)

------------------------------------------------------------------------

## Length Predicates

Efficient length comparisons that short-circuit when possible.

### length=

Return \#t if length of x equals n.

**Signature:** `(length= x n)`

**Parameters:** - **`x`** — Sequence to measure - **`n`** — Expected
length

**Examples:**

    arl> (length= (list 1 2 3) 3)  ; => #t
    #> TRUE
    arl> (length= (list 1 2) 3)    ; => #f
    #> FALSE

**See also:** [length\>](#length-gt), [length\<](#length-lt),
[empty?](https://willbrannon.com/arl/articles/lang-types.html#empty-p)

------------------------------------------------------------------------

### length\>

Return \#t if length of x is greater than n.

**Signature:** `(length> x n)`

**Parameters:** - **`x`** — Sequence to measure - **`n`** — Length to
compare against

**Examples:**

    arl> (length> (list 1 2 3) 2)  ; => #t
    #> TRUE
    arl> (length> (list 1 2) 3)    ; => #f
    #> FALSE

**See also:** [length=](#length-eq), [length\<](#length-lt)

------------------------------------------------------------------------

### length\<

Return \#t if length of x is less than n.

**Signature:** `(length< x n)`

**Parameters:** - **`x`** — Sequence to measure - **`n`** — Length to
compare against

**Examples:**

    arl> (length< (list 1 2) 3)    ; => #t
    #> TRUE
    arl> (length< (list 1 2 3) 2)  ; => #f
    #> FALSE

**See also:** [length=](#length-eq), [length\>](#length-gt)

------------------------------------------------------------------------

## Search and Deduplication

Functions for finding elements and removing duplicates.

### find

Return first element matching predicate, or \#f if not found.

**Signature:** `(find pred lst)`

**Parameters:** - **`pred`** — Predicate function to test each element -
**`lst`** — List to search

**Examples:**

    arl> (find even? (list 1 3 4 5))  ; => 4
    #> 4
    arl> (find even? (list 1 3 5))    ; => #f
    #> FALSE

**See also:** [member](#member), [contains?](#contains-p),
[filter](https://willbrannon.com/arl/articles/lang-functional.html#filter)

------------------------------------------------------------------------

### distinct

Remove duplicates preserving first-occurrence order. Uses R’s
identical() for comparison.

**Signature:** `(distinct lst)`

**Parameters:** - **`lst`** — List to deduplicate

**Examples:**

    arl> (distinct (list 1 2 1 3 2))  ; => (1 2 3)
    #> (1 2 3)

**See also:**
[filter](https://willbrannon.com/arl/articles/lang-functional.html#filter)

------------------------------------------------------------------------

## Splitting

Functions for splitting sequences at positions or predicates.

### split-at

Split list into two parts at index n. Returns a list of (take n lst) and
(drop n lst).

**Signature:** `(split-at n lst)`

**Parameters:** - **`n`** — Index to split at - **`lst`** — List to
split

**Examples:**

    arl> (split-at 2 (list 'a 'b 'c 'd))  ; => ((a b) (c d))
    #> ((a b) (c d))

**See also:** [split-with](#split-with), [take](#take), [drop](#drop)

------------------------------------------------------------------------

### split-with

Split list into two parts: elements satisfying pred at the start, and
the rest. Returns (take-while pred lst) and (drop-while pred lst).

**Signature:** `(split-with pred lst)`

**Parameters:** - **`pred`** — Predicate function - **`lst`** — List to
split

**Examples:**

    arl> (split-with even? (list 2 4 1 3))  ; => ((2 4) (1 3))
    #> ((2 4) (1 3))

**See also:** [split-at](#split-at), [take-while](#take-while),
[drop-while](#drop-while)

------------------------------------------------------------------------

### interpose

Insert separator between every element of list.

**Signature:** `(interpose sep lst)`

**Parameters:** - **`sep`** — Value to insert between elements -
**`lst`** — List to interpose into

**Examples:**

    arl> (interpose 0 (list 1 2 3))  ; => (1 0 2 0 3)
    #> (1 0 2 0 3)

**See also:** [flatten](#flatten)

------------------------------------------------------------------------

### partition-by

Split list into runs of consecutive elements producing the same key.
Different from partition which splits by size.

**Signature:** `(partition-by fn lst)`

**Parameters:** - **`fn`** — Key function applied to each element -
**`lst`** — List to partition

**Examples:**

    arl> (partition-by even? (list 2 4 1 3 6))  ; => ((2 4) (1 3) (6))
    #> ((2 4) (1 3) (6))

**See also:** [partition](#partition), [split-with](#split-with)

------------------------------------------------------------------------

## Sorting

General-purpose sorting with user-supplied comparator functions.
`list-sort` uses quicksort (not stable); `stable-sort` uses merge sort
and preserves the relative order of equal elements. `merge-sorted`
combines two already-sorted lists.

### list-sort

Sort list using comparison function (simple quicksort implementation).

**Signature:** `(list-sort lst comparator)`

**Parameters:** - **`lst`** — List to sort - **`comparator`** — Binary
predicate returning \#t when first arg should come before second

**Examples:**

    arl> (list-sort (list 3 1 4 1 5) <)        ; => (1 1 3 4 5)
    #> (1 1 3 4 5)
    arl> (list-sort (list "b" "a" "c")
    arl>       (lambda (a b) (< a b)))   ; => ("a" "b" "c")
    #> ("a" "b" "c")

**See also:** [stable-sort](#stable-sort), [sort-by](#sort-by)

------------------------------------------------------------------------

### sort-by

Sort list by applying key-fn to each element, then comparing.

**Signature:** `(sort-by lst key-fn comparator)`

**Parameters:** - **`lst`** — List to sort - **`key-fn`** — Function to
extract a comparison key from each element - **`comparator`** — Binary
predicate applied to extracted keys

**Examples:**

    arl> (sort-by (list (list "b" 2) (list "a" 1)) car
    arl>          (lambda (a b) (< a b)))  ; => (("a" 1) ("b" 2))
    #> (("a" 1) ("b" 2))
    arl> (sort-by (list 3 -1 2) abs <)     ; => (-1 2 3)
    #> (-1 2 3)

**See also:** [list-sort](#list-sort), [stable-sort](#stable-sort)

------------------------------------------------------------------------

### merge-sorted

Merge two sorted lists into one sorted list. Stable: when elements are
equal (neither comparator direction is true), takes from list1 first.

**Signature:** `(merge-sorted list1 list2 comparator)`

**Parameters:** - **`list1`** — First sorted list - **`list2`** — Second
sorted list - **`comparator`** — Binary predicate used to order elements

**Examples:**

    arl> (merge-sorted (list 1 3 5) (list 2 4 6) <)  ; => (1 2 3 4 5 6)
    #> (1 2 3 4 5 6)
    arl> (merge-sorted (list 1 2) (list) <)           ; => (1 2)
    #> (1 2)

**See also:** [stable-sort](#stable-sort), [list-sort](#list-sort)

------------------------------------------------------------------------

### stable-sort

Stable sort - preserves order of equal elements. Uses merge sort
internally.

**Signature:** `(stable-sort lst comparator)`

**Parameters:** - **`lst`** — List to sort - **`comparator`** — Binary
predicate returning \#t when first arg should come before second

**Examples:**

    arl> (stable-sort (list 3 1 4 1 5) <)  ; => (1 1 3 4 5)
    #> (1 1 3 4 5)

**Note:** Preserves the relative order of elements that compare equal,
unlike `list-sort`.

**See also:** [list-sort](#list-sort), [merge-sorted](#merge-sorted),
[sort-by](#sort-by)

------------------------------------------------------------------------
