# Standard Library: Collections and Data Structures

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## Dictionary (Hash Table)

Dictionaries are mutable key-value stores backed by R environments. Keys
must be strings, symbols, or keywords; values can be anything.

### dict-new

Create a new empty dictionary environment.

**Signature:** `(dict-new)`

------------------------------------------------------------------------

### dict-key-to-name

Convert key to string name for dict storage.

**Signature:** `(dict-key-to-name key)`

**Parameters:** - **`key`** — String, symbol, or keyword to convert

------------------------------------------------------------------------

### dict

Create a hash-backed dictionary from key/value pairs.

**Signature:** `(dict args...)`

**Parameters:** - **`args`** — Named key/value pairs (e.g. :x 1 :y 2)

**Examples:**

    arl> (dict :x 1 :y 2)           ; => dict with keys "x" and "y"
    #> 1 2
    arl> (dict)                      ; => empty dict
    arl> (define d (dict :name "Alice" :age 30))
    #> "Alice" 30
    arl> (dict-get d "name")         ; => "Alice"
    #> "Alice"

**See also:** [hash](#hash)

------------------------------------------------------------------------

### hash

Alias for dict. Create a dictionary from key/value pairs.

**Signature:** `(hash . args)`

**Examples:**

    arl> (hash :x 1 :y 2)           ; => dict with x=1, y=2
    #> 1 2

**Note:** Alias for `dict`.

**See also:** [dict](#dict)

------------------------------------------------------------------------

### dict?

Return \#t if x is a dictionary.

**Signature:** `(dict? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (dict? (dict :x 1))        ; => #t
    #> TRUE
    arl> (dict? (list 1 2))         ; => #f
    #> FALSE
    arl> (dict? 42)                 ; => #f
    #> FALSE

------------------------------------------------------------------------

### dict-keys-ordered

Return ordered keys vector (internal helper).

**Signature:** `(dict-keys-ordered dict)`

**Parameters:** - **`dict`** — Dictionary to retrieve keys from

------------------------------------------------------------------------

### dict-get

Get value for key or default if missing.

**Signature:** `(dict-get dict key rest...)`

**Parameters:** - **`dict`** — Dictionary to look up in - **`key`** —
Key to retrieve (string, symbol, or keyword) - **`rest`** — Optional
default value if key is missing (defaults to \#nil)

**Examples:**

    arl> (define d (dict :x 1 :y 2))
    #> 1 2
    arl> (dict-get d "x")           ; => 1
    #> 1
    arl> (dict-get d "z")           ; => #nil
    arl> (dict-get d "z" 99)        ; => 99  (default value)
    #> 99

**See also:** [dict-set](#dict-set), [dict-has?](#dict-has-p)

------------------------------------------------------------------------

### dict-set

Set key to value in dict and return dict.

**Signature:** `(dict-set dict key value)`

**Parameters:** - **`dict`** — Dictionary to modify - **`key`** — Key to
set (string, symbol, or keyword) - **`value`** — Value to associate with
the key

**Examples:**

    arl> (define d (dict))
    arl> (dict-set d "x" 42)        ; => d (mutated, x=42)
    #> 42
    arl> (dict-get d "x")           ; => 42
    #> 42

**See also:** [dict-get](#dict-get), [dict-remove](#dict-remove)

------------------------------------------------------------------------

### dict-remove

Remove key from dict and return dict.

**Signature:** `(dict-remove dict key)`

**Parameters:** - **`dict`** — Dictionary to modify - **`key`** — Key to
remove (string, symbol, or keyword)

**Examples:**

    arl> (define d (dict :x 1 :y 2))
    #> 1 2
    arl> (dict-remove d "x")        ; => d (mutated, x removed)
    #> 2
    arl> (dict-has? d "x")          ; => #f
    #> FALSE

**See also:** [dict-set](#dict-set)

------------------------------------------------------------------------

### dict-keys

Return a list of dict keys.

**Signature:** `(dict-keys dict)`

**Parameters:** - **`dict`** — Dictionary to retrieve keys from

**Examples:**

    arl> (define d (dict :x 1 :y 2))
    #> 1 2
    arl> (dict-keys d)              ; => ("x" "y")
    #> ("x" "y")
    arl> (dict-keys (dict))         ; => ()
    #> ()

**See also:** [dict-values](#dict-values)

------------------------------------------------------------------------

### dict-values

Return a list of dict values.

**Signature:** `(dict-values dict)`

**Parameters:** - **`dict`** — Dictionary to retrieve values from

**Examples:**

    arl> (define d (dict :x 1 :y 2))
    #> 1 2
    arl> (dict-values d)            ; => (1 2)
    #> (1 2)

**See also:** [dict-keys](#dict-keys)

------------------------------------------------------------------------

### dict-has?

Return \#t if dict contains key.

**Signature:** `(dict-has? dict key)`

**Parameters:** - **`dict`** — Dictionary to check - **`key`** — Key to
look for (string, symbol, or keyword)

**Examples:**

    arl> (define d (dict :x 1 :y 2))
    #> 1 2
    arl> (dict-has? d "x")          ; => #t
    #> TRUE
    arl> (dict-has? d "z")          ; => #f
    #> FALSE

**See also:** [dict-get](#dict-get)

------------------------------------------------------------------------

### dict-merge

Merge dicts, later values override earlier.

**Signature:** `(dict-merge dicts...)`

**Parameters:** - **`dicts`** — Dictionaries to merge in order (later
values win)

**Examples:**

    arl> (define a (dict :x 1 :y 2))
    #> 1 2
    arl> (define b (dict :y 99 :z 3))
    #> 99 3
    arl> (define m (dict-merge a b))
    #> 1 99 3
    arl> (dict-get m "y")           ; => 99  (b overrides a)
    #> 99
    arl> (dict-get m "z")           ; => 3
    #> 3

**See also:** [dict](#dict), [dict-set](#dict-set)

------------------------------------------------------------------------

## Dictionary Transformation

Functions for updating, mapping, filtering, and converting dictionaries.

### dict-update

Apply fn to the value at key (or default if missing). Mutates and
returns dict.

**Signature:** `(dict-update d key fn rest...)`

**Parameters:** - **`d`** — Dictionary to update - **`key`** — Key to
update - **`fn`** — Function to apply to the current value - **`rest`**
— Optional default value if key is missing

**Examples:**

    arl> (dict-update (dict :x 1) "x" inc)  ; => dict with x=2
    #> 2

**See also:** [dict-set](#dict-set), [dict-get](#dict-get)

------------------------------------------------------------------------

### dict-map

Map fn over dict entries. fn takes key and value, returns new value.
Returns a new dict.

**Signature:** `(dict-map fn d)`

**Parameters:** - **`fn`** — Function taking key (string) and value,
returning new value - **`d`** — Dictionary to map over

**Examples:**

    arl> (dict-map (lambda (k v) (* v 10)) (dict :x 1))  ; => dict with x=10
    #> 10

**See also:** [dict-filter](#dict-filter),
[dict-for-each](#dict-for-each)

------------------------------------------------------------------------

### dict-filter

Filter dict entries. pred takes key and value, returns \#t to keep.
Returns a new dict.

**Signature:** `(dict-filter pred d)`

**Parameters:** - **`pred`** — Predicate function taking key (string)
and value - **`d`** — Dictionary to filter

**Examples:**

    arl> (dict-filter (lambda (k v) (> v 1)) (dict :x 1 :y 2))  ; => dict with y=2
    #> 2

**See also:** [dict-map](#dict-map), [dict-for-each](#dict-for-each)

------------------------------------------------------------------------

### dict-for-each

Iterate over dict entries for side effects. fn takes key and value.
Returns \#nil.

**Signature:** `(dict-for-each fn d)`

**Parameters:** - **`fn`** — Function taking key (string) and value -
**`d`** — Dictionary to iterate over

**Examples:**

    arl> (dict-for-each (lambda (k v) (print v)) (dict :x 1))  ; prints 1
    #> [1] 1

**See also:** [dict-map](#dict-map), [dict-filter](#dict-filter)

------------------------------------------------------------------------

### dict-\>alist

Convert dict to association list of (key value) pairs.

**Signature:** `(dict->alist d)`

**Parameters:** - **`d`** — Dictionary to convert

**Examples:**

    arl> (dict->alist (dict :x 1 :y 2))  ; => (("x" 1) ("y" 2))
    #> (("x" 1) ("y" 2))

**See also:** [alist-\>dict](#alist-to-dict)

------------------------------------------------------------------------

### alist-\>dict

Convert association list of (key value) pairs to dict.

**Signature:** `(alist->dict alist)`

**Parameters:** - **`alist`** — List of (key value) pairs

**Examples:**

    arl> (alist->dict (list (list "x" 1) (list "y" 2)))  ; => dict with x=1 y=2
    #> 1 2

**See also:** [dict-\>alist](#dict-to-alist)

------------------------------------------------------------------------

## Set (Hash-Backed)

Sets are mutable collections of unique items backed by R environments.
Any value can be a set element; uniqueness is determined by
serialization.

### set-new

Create a new empty set environment.

**Signature:** `(set-new)`

------------------------------------------------------------------------

### set-key

Convert value to string key for set storage via serialization.

**Signature:** `(set-key value)`

**Parameters:** - **`value`** — Value to serialize into a string key

------------------------------------------------------------------------

### set

Create a hash-backed set of unique items.

**Signature:** `(set args...)`

**Parameters:** - **`args`** — Items to include in the set, or a single
list to convert

**Examples:**

    arl> (set 1 2 3)                ; => set of {1, 2, 3}
    #> 3 1 2
    arl> (set 1 1 2)                ; => set of {1, 2} (duplicates removed)
    #> 1 2
    arl> (set)                      ; => empty set
    arl> (set '(a b c))             ; => set from list
    #> c b a

------------------------------------------------------------------------

### set?

Return \#t if x is a set.

**Signature:** `(set? x)`

**Parameters:** - **`x`** — Value to test

**Examples:**

    arl> (set? (set 1 2))           ; => #t
    #> TRUE
    arl> (set? (list 1 2))          ; => #f
    #> FALSE
    arl> (set? 42)                  ; => #f
    #> FALSE

------------------------------------------------------------------------

### set-add

Add item to set and return set.

**Signature:** `(set-add set item)`

**Parameters:** - **`set`** — Set to add to - **`item`** — Value to add

**Examples:**

    arl> (define s (set 1 2))
    #> 1 2
    arl> (set-add s 3)              ; => s (mutated, now {1, 2, 3})
    #> 3 1 2
    arl> (set-contains? s 3)        ; => #t
    #> TRUE

**See also:** [set-remove](#set-remove)

------------------------------------------------------------------------

### set-remove

Remove item from set and return set.

**Signature:** `(set-remove set item)`

**Parameters:** - **`set`** — Set to remove from - **`item`** — Value to
remove

**Examples:**

    arl> (define s (set 1 2 3))
    #> 3 1 2
    arl> (set-remove s 2)           ; => s (mutated, now {1, 3})
    #> 3 1
    arl> (set-contains? s 2)        ; => #f
    #> FALSE

**See also:** [set-add](#set-add)

------------------------------------------------------------------------

### set-contains?

Return \#t if set contains item.

**Signature:** `(set-contains? set item)`

**Parameters:** - **`set`** — Set to search in - **`item`** — Value to
look for

**Examples:**

    arl> (define s (set 1 2 3))
    #> 3 1 2
    arl> (set-contains? s 2)        ; => #t
    #> TRUE
    arl> (set-contains? s 99)       ; => #f
    #> FALSE

------------------------------------------------------------------------

### set-union

Return union of two sets.

**Signature:** `(set-union a b)`

**Parameters:** - **`a`** — First set - **`b`** — Second set

**Examples:**

    arl> (define a (set 1 2 3))
    #> 3 1 2
    arl> (define b (set 3 4 5))
    #> 3 5 4
    arl> (define u (set-union a b))
    #> 1 3 2 5 4
    arl> (set-contains? u 1)        ; => #t
    #> TRUE
    arl> (set-contains? u 5)        ; => #t
    #> TRUE

**See also:** [set-intersection](#set-intersection),
[set-difference](#set-difference)

------------------------------------------------------------------------

### set-intersection

Return intersection of two sets.

**Signature:** `(set-intersection a b)`

**Parameters:** - **`a`** — First set - **`b`** — Second set

**Examples:**

    arl> (define a (set 1 2 3))
    #> 3 1 2
    arl> (define b (set 2 3 4))
    #> 3 2 4
    arl> (define i (set-intersection a b))
    #> 3 2
    arl> (set-contains? i 2)        ; => #t
    #> TRUE
    arl> (set-contains? i 1)        ; => #f
    #> FALSE

**See also:** [set-union](#set-union), [set-difference](#set-difference)

------------------------------------------------------------------------

### set-difference

Return items in a that are not in b.

**Signature:** `(set-difference a b)`

**Parameters:** - **`a`** — Set to take items from - **`b`** — Set whose
items are excluded

**Examples:**

    arl> (define a (set 1 2 3))
    #> 3 1 2
    arl> (define b (set 2 3 4))
    #> 3 2 4
    arl> (define d (set-difference a b))
    #> 1
    arl> (set-contains? d 1)        ; => #t
    #> TRUE
    arl> (set-contains? d 2)        ; => #f
    #> FALSE

**See also:** [set-union](#set-union),
[set-intersection](#set-intersection)

------------------------------------------------------------------------

### set-copy-into

Copy items from source into target (internal helper).

**Signature:** `(set-copy-into target source)`

**Parameters:** - **`target`** — Set to copy items into - **`source`** —
Set to copy items from

------------------------------------------------------------------------

## Set Conversion and Transformation

Functions for converting between sets and lists, and for transforming
sets.

### set-\>list

Extract set elements as a list.

**Signature:** `(set->list s)`

**Parameters:** - **`s`** — Set to convert

**Examples:**

    arl> (set->list (set 1 2 3))  ; => (1 2 3) (order may vary)
    #> (3 1 2)

**See also:** [list-\>set](#list-to-set)

------------------------------------------------------------------------

### list-\>set

Create a set from a list.

**Signature:** `(list->set lst)`

**Parameters:** - **`lst`** — List of values to include in the set

**Examples:**

    arl> (list->set '(1 2 3 2 1))  ; => set of {1, 2, 3}
    #> 3 1 2

**See also:** [set-\>list](#set-to-list), [set](#set)

------------------------------------------------------------------------

### set-size

Return the number of elements in a set.

**Signature:** `(set-size s)`

**Parameters:** - **`s`** — Set to measure

**Examples:**

    arl> (set-size (set 1 2 3))  ; => 3
    #> 3
    arl> (set-size (set))         ; => 0
    #> 0

**See also:** [set?](#set-p)

------------------------------------------------------------------------

### set-map

Apply function to each element, return new set of results.

**Signature:** `(set-map fn s)`

**Parameters:** - **`fn`** — Function to apply to each element - **`s`**
— Set to map over

**Examples:**

    arl> (set-map inc (set 1 2 3))  ; => set of {2, 3, 4}
    #> 3 2 4

**See also:** [set-filter](#set-filter)

------------------------------------------------------------------------

### set-filter

Keep only elements matching predicate, return new set.

**Signature:** `(set-filter pred s)`

**Parameters:** - **`pred`** — Predicate function to test each element -
**`s`** — Set to filter

**Examples:**

    arl> (set-filter even? (set 1 2 3 4))  ; => set of {2, 4}
    #> 2 4

**See also:** [set-map](#set-map)

------------------------------------------------------------------------

## Struct Definition

### defstruct

Define a struct constructor, predicate, and accessors.

**Signature:** `(defstruct name fields)`

**Parameters:** - **`name`** — Name for the struct type - **`fields`** —
List of field name symbols

**Examples:**

    arl> ;; Define a Point struct
    arl> (defstruct Point (x y))
    #> <function>

    arl> ;; Constructor: make-Point
    arl> (define p (make-Point 3 4))
    #> $x
    #> [1] 3
    #> 
    #> $y
    #> [1] 4
    #> 
    #> attr(,"class")
    #> [1] "Point"

    arl> ;; Predicate: Point?
    arl> (Point? p)                  ; => #t
    #> TRUE
    arl> (Point? 42)                 ; => #f
    #> FALSE

    arl> ;; Accessors: Point-x, Point-y
    arl> (Point-x p)                 ; => 3
    #> 3
    arl> (Point-y p)                 ; => 4
    #> 4

**Note:** `defstruct` generates a constructor (`make-Name`), a type
predicate (`Name?`), and field accessors (`Name-field`) for each field.
The struct is backed by an R named list with an S3 class.

**See also:** [dict](#dict)

------------------------------------------------------------------------
