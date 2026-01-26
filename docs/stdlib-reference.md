# Standard Library Reference

Rye ships with a small standard library implemented in R, plus additional Rye
source modules. The REPL loads the base R stdlib by default; you can also load
modules from `inst/rye/` when you want more macros and helpers.

## Loading

```lisp
; Load focused modules
(load "control")   ; when/unless/and/or/cond/case
(load "binding")   ; let/let*/letrec
(load "looping")   ; while/for
(load "threading") ; -> and ->>
(load "error")     ; try/catch/finally
```

From R, you can load all modules with:

```r
env <- rye_load_stdlib(load_files = TRUE)
```

## Core list operations

`car`, `cdr`, `cons`, `call`, `list*`, `append`, `reverse`, `apply`,
`first`, `rest`, `last`, `nth`

## Higher-order functions

`map`, `mapcat`, `filter`, `remove`, `reduce`, `foldl`, `foldr`,
`every?`, `any?`, `complement`, `compose`, `partial`

## Sequence helpers

`take`, `drop`, `take-while`, `drop-while`, `partition`, `flatten`, `zip`,
`repeatedly`, `repeat`

## Predicates

`null?`, `nil?`, `list?`, `pair?`, `symbol?`, `keyword?`,
`number?`, `string?`, `vector?`, `true?`, `false?`, `fn?`, `callable?`

## Control flow macros

`when`, `unless`, `and`, `or`, `cond`, `case`

## Binding and looping

`let`, `let*`, `letrec`, `while`, `for`

## Threading macros

`->`, `->>`

## Error handling

`try`, `catch`, `finally`, `error`, `warn`, `assert`, `trace`, `try*`

## String and I/O

`str`, `string-join`, `string-split`, `trim`, `format`,
`read-line`, `display`, `println`

## Macro system

`gensym`, `macro?`, `eval`, `macroexpand`, `macroexpand-1`

## Interop helpers

`dict`, `hash`, `r/call`, `identity`

## Arithmetic and comparison

`+`, `-`, `*`, `/`, `%`, `=`, `<`, `>`, `<=`, `>=`, `not`

## Source mapping

The stdlib is organized by topic in `inst/rye/`. The base R implementation
lives in `R/stdlib.R`.

- `binding.rye`
- `control.rye`
- `error.rye`
- `looping.rye`
- `threading.rye`

If you’re looking for implementation details, these files are the source of
truth for the stdlib definitions.

# Rye Standard Library Reference

This document provides a comprehensive reference for all functions and macros in the Rye standard library.

## Table of Contents

1. [List Operations](#list-operations)
2. [Higher-Order Functions](#higher-order-functions)
3. [Sequence Helpers](#sequence-helpers)
4. [Predicates](#predicates)
5. [Control Flow Macros](#control-flow-macros)
6. [Binding Macros](#binding-macros)
7. [Looping Macros](#looping-macros)
8. [Threading Macros](#threading-macros)
9. [Error Handling](#error-handling)
10. [String and I/O](#string-and-io)
11. [Arithmetic and Comparison](#arithmetic-and-comparison)
12. [Macro and Eval](#macro-and-eval)
13. [Interop Helpers](#interop-helpers)
14. [Convenience Functions](#convenience-functions)

---

## List Operations

### car

Returns the first element of a list.

**Signature:** `(car lst)`

**Examples:**
```lisp
(car (list 1 2 3))  ; => 1
(car ())            ; => #nil
(car (list "a" "b")) ; => "a"
```

**See also:** cdr, cons, first

---

### cdr

Returns all elements except the first (the "rest" of the list).

**Signature:** `(cdr lst)`

**Examples:**
```lisp
(cdr (list 1 2 3))  ; => (2 3)
(cdr (list 1))      ; => ()
(cdr ())            ; => ()
```

**See also:** car, cons, rest

---

### cons

Adds an element to the front of a list.

**Signature:** `(cons elem lst)`

**Examples:**
```lisp
(cons 1 (list 2 3))     ; => (1 2 3)
(cons "a" ())           ; => ("a")
(cons (list 1 2) (list 3)) ; => ((1 2) 3)
```

**See also:** car, cdr, append

---

### call

Converts a list to a call expression.

**Signature:** `(call lst)`

**Examples:**
```lisp
(call (list + 1 2))  ; => (+ 1 2)
(eval (call (list * 3 4))) ; => 12
```

**See also:** eval, apply

---

### list*

Creates a list from elements, with the last argument providing the tail.

**Signature:** `(list* elem1 elem2 ... tail)`

**Examples:**
```lisp
(list* 1 (list 2 3))     ; => (1 2 3)
(list* 1 2 (list 3 4))   ; => (1 2 3 4)
(list* 1 ())             ; => (1)
```

**See also:** list, cons, append

---

### append

Concatenates two lists.

**Signature:** `(append lst1 lst2)`

**Examples:**
```lisp
(append (list 1 2) (list 3 4))  ; => (1 2 3 4)
(append () (list 1))            ; => (1)
(append (list "a") (list "b" "c")) ; => ("a" "b" "c")
```

**See also:** cons, list*

---

### reverse

Reverses a list.

**Signature:** `(reverse lst)`

**Examples:**
```lisp
(reverse (list 1 2 3))  ; => (3 2 1)
(reverse ())            ; => ()
(reverse (list "a" "b")) ; => ("b" "a")
```

---

### apply

Applies a function to a list of arguments.

**Signature:** `(apply fn args)`

**Examples:**
```lisp
(apply + (list 1 2 3))  ; => 6
(apply max (list 5 2 9 1)) ; => 9
(apply str (list "a" "b" "c")) ; => "abc"
```

**See also:** map, reduce

---

## Higher-Order Functions

### map

Applies a function to each element of a list, returning a new list.

**Signature:** `(map fn lst)`

**Examples:**
```lisp
(map (lambda (x) (* x 2)) (list 1 2 3))  ; => (2 4 6)
(map str (list 1 2 3))                   ; => ("1" "2" "3")
(map first (list (list 1 2) (list 3 4))) ; => (1 3)
```

**See also:** filter, mapcat, reduce

---

### mapcat

Maps a function over a list and concatenates the results.

**Signature:** `(mapcat fn lst)`

**Examples:**
```lisp
(mapcat (lambda (x) (list x x)) (list 1 2 3))  ; => (1 1 2 2 3 3)
(mapcat identity (list (list 1 2) (list 3 4))) ; => (1 2 3 4)
```

**See also:** map, flatten

---

### filter

Returns a list of elements that satisfy a predicate.

**Signature:** `(filter pred lst)`

**Examples:**
```lisp
(filter (lambda (x) (> x 5)) (list 1 10 3 8))  ; => (10 8)
(filter number? (list 1 "a" 2 "b"))            ; => (1 2)
(filter even? (list 1 2 3 4 5 6))              ; => (2 4 6)
```

**See also:** remove, take-while, drop-while

---

### remove

Returns a list of elements that do NOT satisfy a predicate.

**Signature:** `(remove pred lst)`

**Examples:**
```lisp
(remove (lambda (x) (> x 5)) (list 1 10 3 8))  ; => (1 3)
(remove string? (list 1 "a" 2 "b"))            ; => (1 2)
```

**See also:** filter

---

### reduce

Reduces a list to a single value by repeatedly applying a function.

**Signature:** `(reduce fn lst)` or `(reduce fn lst init)`

**Examples:**
```lisp
(reduce + (list 1 2 3 4))       ; => 10
(reduce * (list 2 3 4))         ; => 24
(reduce + (list 1 2 3) 10)      ; => 16
```

**See also:** foldl, foldr, map

---

### foldl

Left fold - reduces from left to right.

**Signature:** `(foldl fn lst)`

**Examples:**
```lisp
(foldl - (list 1 2 3))  ; => -4  (equivalent to ((1 - 2) - 3))
(foldl / (list 8 2 2))  ; => 2   (equivalent to ((8 / 2) / 2))
```

**See also:** foldr, reduce

---

### foldr

Right fold - reduces from right to left.

**Signature:** `(foldr fn lst)`

**Examples:**
```lisp
(foldr - (list 1 2 3))  ; => 2   (equivalent to (1 - (2 - 3)))
(foldr / (list 8 2 2))  ; => 8   (equivalent to (8 / (2 / 2)))
```

**See also:** foldl, reduce

---

### every?

Returns true if all elements satisfy the predicate.

**Signature:** `(every? pred lst)`

**Examples:**
```lisp
(every? number? (list 1 2 3))         ; => #t
(every? (lambda (x) (> x 0)) (list 1 2 3)) ; => #t
(every? even? (list 2 4 5))           ; => #f
```

**See also:** any?, filter

---

### any?

Returns true if any element satisfies the predicate.

**Signature:** `(any? pred lst)`

**Examples:**
```lisp
(any? number? (list "a" 1 "b"))       ; => #t
(any? (lambda (x) (> x 10)) (list 1 2 3)) ; => #f
(any? even? (list 1 3 5 6))           ; => #t
```

**See also:** every?, filter

---

## Sequence Helpers

### take

Takes the first n elements from a list.

**Signature:** `(take n lst)`

**Examples:**
```lisp
(take 2 (list 1 2 3 4))  ; => (1 2)
(take 0 (list 1 2 3))    ; => ()
(take 10 (list 1 2))     ; => (1 2)
```

**See also:** drop, take-while

---

### drop

Drops the first n elements from a list.

**Signature:** `(drop n lst)`

**Examples:**
```lisp
(drop 2 (list 1 2 3 4))  ; => (3 4)
(drop 0 (list 1 2 3))    ; => (1 2 3)
(drop 10 (list 1 2))     ; => ()
```

**See also:** take, drop-while

---

### take-while

Takes elements while they satisfy a predicate.

**Signature:** `(take-while pred lst)`

**Examples:**
```lisp
(take-while (lambda (x) (< x 5)) (list 1 2 3 6 4))  ; => (1 2 3)
(take-while number? (list 1 2 "a" 3))               ; => (1 2)
```

**See also:** take, drop-while, filter

---

### drop-while

Drops elements while they satisfy a predicate.

**Signature:** `(drop-while pred lst)`

**Examples:**
```lisp
(drop-while (lambda (x) (< x 5)) (list 1 2 3 6 4))  ; => (6 4)
(drop-while number? (list 1 2 "a" 3))               ; => ("a" 3)
```

**See also:** drop, take-while, filter

---

### partition

Divides a list into chunks of size n.

**Signature:** `(partition n lst)` or `(partition n lst step)`

**Examples:**
```lisp
(partition 2 (list 1 2 3 4))     ; => ((1 2) (3 4))
(partition 3 (list 1 2 3 4 5))   ; => ((1 2 3))  ; incomplete chunks dropped
(partition 2 (list 1 2 3 4 5) 1) ; => ((1 2) (2 3) (3 4) (4 5))  ; overlapping
```

**See also:** take, drop

---

### flatten

Flattens nested lists into a single list.

**Signature:** `(flatten lst)`

**Examples:**
```lisp
(flatten (list 1 (list 2 3) 4))           ; => (1 2 3 4)
(flatten (list (list 1 2) (list 3 4)))    ; => (1 2 3 4)
(flatten (list 1 (list 2 (list 3 4)) 5))  ; => (1 2 3 4 5)
```

**See also:** mapcat, append

---

## Predicates

### list?

Tests if a value is a list.

**Signature:** `(list? x)`

**Examples:**
```lisp
(list? (list 1 2 3))  ; => #t
(list? ())            ; => #t
(list? "hello")       ; => #f
```

**See also:** pair?, null?

---

### pair?

Tests if a value is a non-empty list.

**Signature:** `(pair? x)`

**Examples:**
```lisp
(pair? (list 1))      ; => #t
(pair? (list 1 2 3))  ; => #t
(pair? ())            ; => #f
```

**See also:** list?, null?

---

### null?

Tests if a value is NULL or an empty list.

**Signature:** `(null? x)`

**Examples:**
```lisp
(null? #nil)          ; => #t
(null? ())            ; => #t
(null? (list 1))      ; => #f
```

**See also:** nil?, pair?

---

### nil?

Alias for `null?`.

**Signature:** `(nil? x)`

**See also:** null?

---

### symbol?

Tests if a value is a symbol.

**Signature:** `(symbol? x)`

**Examples:**
```lisp
(symbol? 'foo)        ; => #t
(symbol? "foo")       ; => #f
(symbol? 42)          ; => #f
```

---

### keyword?

Tests if a value is a keyword (`:key` syntax).

**Signature:** `(keyword? x)`

**Examples:**
```lisp
(keyword? :foo)       ; => #t
(keyword? 'foo)       ; => #f
```

---

### number?

Tests if a value is a number.

**Signature:** `(number? x)`

**Examples:**
```lisp
(number? 42)          ; => #t
(number? 3.14)        ; => #t
(number? "42")        ; => #f
```

---

### string?

Tests if a value is a string.

**Signature:** `(string? x)`

**Examples:**
```lisp
(string? "hello")     ; => #t
(string? 'hello)      ; => #f
(string? 42)          ; => #f
```

---

### vector?

Tests if a value is a numeric vector.

**Signature:** `(vector? x)`

**Examples:**
```lisp
(vector? [1 2 3])     ; => #t
(vector? (list 1 2))  ; => #f
```

**Note:** Only numeric vectors are considered vectors by this predicate.

---

### true?

Tests if a value is exactly TRUE.

**Signature:** `(true? x)`

**Examples:**
```lisp
(true? #t)            ; => #t
(true? 1)             ; => #f
```

---

### false?

Tests if a value is exactly FALSE.

**Signature:** `(false? x)`

**Examples:**
```lisp
(false? #f)           ; => #t
(false? 0)            ; => #f
```

---

### fn?

Tests if a value is a function.

**Signature:** `(fn? x)`

**Examples:**
```lisp
(fn? +)               ; => #t
(fn? (lambda (x) x))  ; => #t
(fn? 42)              ; => #f
```

**See also:** callable?

---

### callable?

Alias for `fn?`.

**Signature:** `(callable? x)`

**See also:** fn?

---

## Control Flow Macros

### when

Executes body if test is truthy.

**Signature:** `(when test body...)`

**Examples:**
```lisp
(when (> 5 3)
  (println "5 is greater than 3"))  ; prints message

(when #f
  (println "not printed"))          ; nothing happens
```

**Defined in:** `inst/rye/control.rye`

**See also:** unless, if, cond

---

### unless

Executes body if test is falsy.

**Signature:** `(unless test body...)`

**Examples:**
```lisp
(unless (> 3 5)
  (println "3 is not greater than 5"))  ; prints message

(unless #t
  (println "not printed"))              ; nothing happens
```

**Defined in:** `inst/rye/control.rye`

**See also:** when, if

---

### and

Logical AND with short-circuit evaluation.

**Signature:** `(and expr1 expr2 ...)`

**Examples:**
```lisp
(and #t #t)           ; => #t
(and #t #f)           ; => #f
(and (> 5 3) (< 2 4)) ; => #t
```

**Defined in:** `inst/rye/control.rye`

**See also:** or, not

---

### or

Logical OR with short-circuit evaluation.

**Signature:** `(or expr1 expr2 ...)`

**Examples:**
```lisp
(or #f #t)            ; => #t
(or #f #f)            ; => #f
(or (> 3 5) (< 2 4))  ; => #t
```

**Defined in:** `inst/rye/control.rye`

**See also:** and, not

---

### not

Logical NOT.

**Signature:** `(not expr)`

**Examples:**
```lisp
(not #t)              ; => #f
(not #f)              ; => #t
(not 42)              ; => #f  (only #f is falsy)
```

**See also:** and, or

---

### cond

Multi-way conditional.

**Signature:** `(cond (test1 result1) (test2 result2) ... (else result))`

**Examples:**
```lisp
(cond
  ((> x 10) "big")
  ((> x 5) "medium")
  (else "small"))
```

**Defined in:** `inst/rye/control.rye`

**See also:** case, if, when

---

### case

Pattern matching on a value.

**Signature:** `(case expr (pattern1 result1) (pattern2 result2) ... (else result))`

**Examples:**
```lisp
(case x
  (1 "one")
  (2 "two")
  (3 "three")
  (else "other"))
```

**Defined in:** `inst/rye/control.rye`

**See also:** cond, if

---

## Binding Macros

### let

Creates local bindings (sequential).

**Signature:** `(let ((var1 val1) (var2 val2) ...) body...)`

**Examples:**
```lisp
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

**Defined in:** `inst/rye/binding.rye`

**See also:** let*, letrec

---

### let*

Creates local bindings (each binding can reference previous ones).

**Signature:** `(let* ((var1 val1) (var2 val2) ...) body...)`

**Examples:**
```lisp
(let* ((x 10)
       (y (* x 2)))
  (+ x y))  ; => 30
```

**Defined in:** `inst/rye/binding.rye`

**See also:** let, letrec

---

### letrec

Creates local bindings (allows recursive definitions).

**Signature:** `(letrec ((var1 val1) (var2 val2) ...) body...)`

**Examples:**
```lisp
(letrec ((factorial
          (lambda (n)
            (if (= n 0)
                1
                (* n (factorial (- n 1)))))))
  (factorial 5))  ; => 120
```

**Defined in:** `inst/rye/binding.rye`

**See also:** let, let*

---

## Looping Macros

### while

Executes body repeatedly while test is truthy.

**Signature:** `(while test body...)`

**Examples:**
```lisp
(define i 0)
(while (< i 5)
  (println i)
  (set! i (+ i 1)))
```

**Defined in:** `inst/rye/looping.rye`

**See also:** for

---

### for

Iterates over a sequence.

**Signature:** `(for (var lst) body...)`

**Examples:**
```lisp
(for (x (list 1 2 3))
  (println x))

(for (item items)
  (process item))
```

**Defined in:** `inst/rye/looping.rye`

**See also:** while, map

---

## Threading Macros

### ->

Thread-first macro: threads value through forms as first argument.

**Signature:** `(-> value form1 form2 ...)`

**Examples:**
```lisp
(-> 5
    (+ 3)
    (* 2))  ; => 16  (equivalent to (* (+ 5 3) 2))

(-> data
    (filter even?)
    (map double))
```

**Defined in:** `inst/rye/threading.rye`

**See also:** ->>

---

### ->>

Thread-last macro: threads value through forms as last argument.

**Signature:** `(->> value form1 form2 ...)`

**Examples:**
```lisp
(->> (list 1 2 3 4 5)
     (filter even?)
     (map double)
     (reduce +))  ; => 12
```

**Defined in:** `inst/rye/threading.rye`

**See also:** ->

---

## Error Handling

### error

Raises an error with a message.

**Signature:** `(error message)`

**Examples:**
```lisp
(error "Something went wrong")
(error (str "Invalid value: " x))
```

**See also:** warn, assert, try

---

### warn

Issues a warning with a message.

**Signature:** `(warn message)`

**Examples:**
```lisp
(warn "Deprecated function")
(warn (str "Unusual value: " x))
```

**See also:** error, trace

---

### assert

Asserts that a condition is true, or raises an error.

**Signature:** `(assert test message)`

**Examples:**
```lisp
(assert (> x 0) "x must be positive")
(assert (list? data) "data must be a list")
```

**See also:** error

---

### trace

Prints a debug trace message.

**Signature:** `(trace value label)`

**Examples:**
```lisp
(trace x "x value")
(trace (+ 1 2) "sum")
```

**See also:** warn, display

---

### try / catch / finally

Exception handling.

**Signature:** `(try body... (catch error handler...) (finally cleanup...))`

**Examples:**
```lisp
(try
  (divide x y)
  (catch e
    (println "Error:" e)
    0))

(try
  (risky-operation)
  (catch e
    (println "Failed:" e))
  (finally
    (cleanup)))
```

**Defined in:** `inst/rye/error.rye`

**See also:** error, try*

---

### try*

Low-level exception handling with handlers.

**Signature:** `(try* thunk error-handler finally-handler)`

**Examples:**
```lisp
(try*
  (lambda () (divide x y))
  (lambda (e) (println "Error:" e))
  (lambda () (cleanup)))
```

**See also:** try, catch, finally

---

## String and I/O

### str

Concatenates values into a string.

**Signature:** `(str value...)`

**Examples:**
```lisp
(str "hello" " " "world")  ; => "hello world"
(str "x = " 42)            ; => "x = 42"
(str)                      ; => ""
```

**See also:** string-join, format

---

### string-join

Joins a list of strings with a separator.

**Signature:** `(string-join lst sep)`

**Examples:**
```lisp
(string-join (list "a" "b" "c") "-")  ; => "a-b-c"
(string-join (list "hello" "world") " ")  ; => "hello world"
```

**See also:** string-split, str

---

### string-split

Splits a string by a separator.

**Signature:** `(string-split str sep)`

**Examples:**
```lisp
(string-split "a-b-c" "-")  ; => ["a" "b" "c"]
(string-split "hello world" " ")  ; => ["hello" "world"]
```

**See also:** string-join

---

### trim

Removes leading and trailing whitespace.

**Signature:** `(trim str)`

**Examples:**
```lisp
(trim "  hello  ")  ; => "hello"
(trim "world")      ; => "world"
```

---

### format

Formats a string with values (using R's sprintf).

**Signature:** `(format fmt ...)`

**Examples:**
```lisp
(format "x = %d, y = %d" 10 20)  ; => "x = 10, y = 20"
(format "%.2f" 3.14159)          ; => "3.14"
```

**See also:** str

---

### display

Prints a value (alias: `println`).

**Signature:** `(display value)`

**Examples:**
```lisp
(display "Hello, world!")
(display (+ 1 2))
```

**See also:** println, trace

---

### println

Alias for `display`.

**Signature:** `(println value)`

**See also:** display

---

### read-line

Reads a line from stdin.

**Signature:** `(read-line)`

**Examples:**
```lisp
(define name (read-line))
(println (str "Hello, " name))
```

---

## Arithmetic and Comparison

### %

Modulo operation.

**Signature:** `(% a b)`

**Examples:**
```lisp
(% 10 3)   ; => 1
(% 17 5)   ; => 2
```

---

### =

Equality test.

**Signature:** `(= a b)`

**Examples:**
```lisp
(= 1 1)           ; => #t
(= "a" "a")       ; => #t
(= 1 2)           ; => #f
```

**Note:** All other R arithmetic operators (`+`, `-`, `*`, `/`, `<`, `>`, etc.) are available directly.

---

## Macro and Eval

### gensym

Generates a unique symbol.

**Signature:** `(gensym)` or `(gensym prefix)`

**Examples:**
```lisp
(gensym)       ; => 'g1234
(gensym "tmp") ; => 'tmp5678
```

**See also:** defmacro

---

### macro?

Tests if a symbol names a macro.

**Signature:** `(macro? sym)`

**Examples:**
```lisp
(macro? 'when)   ; => #t
(macro? 'car)    ; => #f
```

**See also:** defmacro

---

### eval

Evaluates an expression.

**Signature:** `(eval expr)` or `(eval expr env)`

**Examples:**
```lisp
(eval '(+ 1 2))  ; => 3
(eval (list '+ 1 2))  ; => 3
```

**See also:** apply, call

---

### macroexpand

Fully expands all macros in an expression.

**Signature:** `(macroexpand expr)` or `(macroexpand expr env)`

**Examples:**
```lisp
(macroexpand '(when #t 42))
; => (if #t (begin 42) #nil)
```

**See also:** macroexpand-1, macro?

---

### macroexpand-1

Expands macros one level.

**Signature:** `(macroexpand-1 expr)` or `(macroexpand-1 expr env)`

**Examples:**
```lisp
(macroexpand-1 '(when #t 42))
; => (if #t (begin 42) #nil)
```

**See also:** macroexpand, macro?

---

## Interop Helpers

### dict

Creates a named list (dictionary).

**Signature:** `(dict key1: value1 key2: value2 ...)`

**Examples:**
```lisp
(dict :name "Alice" :age 30)
; => (name: "Alice" age: 30)
```

**See also:** hash

---

### hash

Alias for `dict`.

**Signature:** `(hash key1: value1 key2: value2 ...)`

**See also:** dict

---

### r/call

Calls an R function with arguments.

**Signature:** `(r/call fn args)`

**Examples:**
```lisp
(r/call "sum" (list 1 2 3))  ; => 6
(r/call 'sqrt (list 16))     ; => 4
```

**Note:** R functions are also available directly in Rye code (e.g., `(sum 1 2 3)`).

---

## Convenience Functions

### identity

Identity function - returns its argument unchanged.

**Signature:** `(identity x)`

**Examples:**
```lisp
(identity 42)        ; => 42
(map identity lst)   ; => lst
```

**See also:** map

---

### first

Alias for `car` - returns first element.

**Signature:** `(first lst)`

**Examples:**
```lisp
(first (list 1 2 3))  ; => 1
```

**See also:** car, rest, last

---

### rest

Alias for `cdr` - returns all but first element.

**Signature:** `(rest lst)`

**Examples:**
```lisp
(rest (list 1 2 3))  ; => (2 3)
```

**See also:** cdr, first

---

### last

Returns the last element of a list.

**Signature:** `(last lst)`

**Examples:**
```lisp
(last (list 1 2 3))  ; => 3
(last (list 42))     ; => 42
(last ())            ; => #nil
```

**See also:** first, nth

---

### nth

Returns the nth element of a list (0-indexed).

**Signature:** `(nth lst n)`

**Examples:**
```lisp
(nth (list 10 20 30) 0)  ; => 10
(nth (list 10 20 30) 1)  ; => 20
(nth (list 10 20 30) 2)  ; => 30
```

**See also:** first, last

---

### complement

Returns the negation of a predicate function.

**Signature:** `(complement pred)`

**Examples:**
```lisp
(define odd? (complement even?))
(filter odd? (list 1 2 3 4))  ; => (1 3)
```

**See also:** not, filter

---

### compose

Composes two functions (right-to-left application).

**Signature:** `(compose f g)`

**Examples:**
```lisp
(define double (lambda (x) (* x 2)))
(define add-one (lambda (x) (+ x 1)))
(define f (compose add-one double))
(f 5)  ; => 11  (equivalent to (add-one (double 5)))
```

**See also:** partial

---

### repeatedly

Calls a function n times, collecting results.

**Signature:** `(repeatedly n fn)`

**Examples:**
```lisp
(repeatedly 3 random)  ; => (0.123 0.456 0.789)
(repeatedly 5 (lambda () 42))  ; => (42 42 42 42 42)
```

**See also:** repeat, map

---

### repeat

Repeats a value n times.

**Signature:** `(repeat n value)`

**Examples:**
```lisp
(repeat 3 "x")  ; => ("x" "x" "x")
(repeat 5 0)    ; => (0 0 0 0 0)
```

**See also:** repeatedly

---

### zip

Combines multiple lists element-wise.

**Signature:** `(zip lst1 lst2 ...)`

**Examples:**
```lisp
(zip (list 1 2 3) (list "a" "b" "c"))
; => ((1 "a") (2 "b") (3 "c"))

(zip (list 1 2) (list "a" "b") (list #t #f))
; => ((1 "a" #t) (2 "b" #f))
```

**See also:** map

---

### partial

Partially applies arguments to a function.

**Signature:** `(partial fn arg1 arg2 ...)`

**Examples:**
```lisp
(define add-five (partial + 5))
(add-five 10)  ; => 15

(define multiply-by-2-3 (partial * 2 3))
(multiply-by-2-3 4)  ; => 24
```

**See also:** compose

---

## Quick Reference by Category

### Core List Operations
`car`, `cdr`, `cons`, `call`, `list*`, `append`, `reverse`, `apply`, `first`, `rest`, `last`, `nth`

### Iteration & Transformation
`map`, `mapcat`, `filter`, `remove`, `reduce`, `foldl`, `foldr`

### Sequence Operations
`take`, `drop`, `take-while`, `drop-while`, `partition`, `flatten`, `zip`

### Predicates
`list?`, `pair?`, `null?`, `nil?`, `symbol?`, `keyword?`, `number?`, `string?`, `vector?`, `true?`, `false?`, `fn?`, `callable?`

### Control Flow
`when`, `unless`, `and`, `or`, `not`, `cond`, `case`

### Looping
`while`, `for`

### Binding
`let`, `let*`, `letrec`

### Error Handling
`error`, `warn`, `assert`, `trace`, `try`, `catch`, `finally`, `try*`

### String Operations
`str`, `string-join`, `string-split`, `trim`, `format`

### I/O
`display`, `println`, `read-line`

### Higher-Order Utilities
`every?`, `any?`, `identity`, `complement`, `compose`, `partial`, `repeatedly`, `repeat`

### Macro System
`gensym`, `macro?`, `eval`, `macroexpand`, `macroexpand-1`

### Interop
`dict`, `hash`, `r/call`

### Threading
`->`, `->>`
