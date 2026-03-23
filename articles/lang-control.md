# Standard Library: Control Flow and Macros

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## Control Flow Macros

### when

Evaluate body forms when test is truthy.

**Signature:** `(when test body...)`

**Parameters:** - **`test`** — Condition to evaluate - **`body`** —
Expressions to evaluate when test is truthy

**Examples:**

    arl> (when #t "yes")          ; => "yes"
    #> "yes"
    arl> (when #f "yes")          ; => #nil
    arl> (when (> 3 2) (+ 1 1))   ; => 2
    #> 2

**See also:** [unless](#unless),
[if](https://willbrannon.com/arl/articles/lang-core.html#if),
[cond](#cond)

------------------------------------------------------------------------

### unless

Evaluate body forms when test is falsy.

**Signature:** `(unless test body...)`

**Parameters:** - **`test`** — Condition to evaluate - **`body`** —
Expressions to evaluate when test is falsy

**Examples:**

    arl> (unless #f "fallback")    ; => "fallback"
    #> "fallback"
    arl> (unless #t "fallback")    ; => #nil
    arl> (unless (= 1 2) "nope")  ; => "nope"
    #> "nope"

**See also:** [when](#when),
[if](https://willbrannon.com/arl/articles/lang-core.html#if)

------------------------------------------------------------------------

## Conditional Macros

### cond

Multi-branch conditional.

**Signature:** `(cond clause rest...)`

**Parameters:** - **`clause`** — First (test body…) clause - **`rest`**
— Additional clauses; last may be (else body…)

**Examples:**

    arl> (cond
    arl>   ((= 1 2) "a")
    arl>   ((= 1 1) "b")
    arl>   (else "c"))            ; => "b"
    #> "b"

    arl> (cond
    arl>   ((> 10 20) "big")
    arl>   (else "small"))        ; => "small"
    #> "small"

**See also:** [case](#case),
[if](https://willbrannon.com/arl/articles/lang-core.html#if),
[when](#when)

------------------------------------------------------------------------

### case

Branch on key equality. Each clause: ((datum …) body …) or (else body
…). Key expression is evaluated only once.

**Signature:** `(case key clause rest...)`

**Parameters:** - **`key`** — Expression to match (evaluated once) -
**`clause`** — First match clause: ((datum…) body…) - **`rest`** —
Additional clauses; last may be (else body…)

**Examples:**

    arl> (case (+ 1 1)
    arl>   ((1) "one")
    arl>   ((2) "two")
    arl>   ((3) "three")
    arl>   (else "other"))        ; => "two"
    #> "two"

    arl> (case 'x
    arl>   ((a b) "ab")
    arl>   ((x y) "xy")
    arl>   (else "?"))            ; => "xy"
    #> "xy"

**Note:** The key expression is evaluated only once. Each clause datum
list can contain multiple values.

**See also:** [cond](#cond),
[if](https://willbrannon.com/arl/articles/lang-core.html#if)

------------------------------------------------------------------------

## Error Handling

### try

Evaluate thunk with error/finally handlers.

**Signature:** `(try thunk [error_handler] [finally_handler])`

**Parameters:** - **`thunk`** — Zero-argument function to execute -
**`error_handler`** — Function receiving error condition (default \#f) -
**`finally_handler`** — Zero-argument cleanup function (default \#f)

**Examples:**

    arl> (try (lambda () 42))                           ; => 42
    #> 42
    arl> (try (lambda () (stop "oops"))
    arl>       :error_handler (lambda (e) "caught"))      ; => "caught"
    #> "caught"
    arl> (try (lambda () 1)
    arl>       :finally_handler (lambda () (display "done")))  ; => 1
    #> "done" 
    #> 1

**Note:** Low-level functional interface to R’s tryCatch.

**See also:** [try-catch](#try-catch)

------------------------------------------------------------------------

### try-catch

Macro wrapper around try with catch/finally.

**Signature:** `(try-catch body clauses...)`

**Parameters:** - **`body`** — Expression to evaluate - **`clauses`** —
Optional (catch var body…) and/or (finally body…) clauses

**Examples:**

    arl> ;; Basic try with catch
    arl> (try-catch (/ 1 0)
    arl>   (catch e (string-append "caught: " ($ e "message"))))
    #> Inf

    arl> ;; try with finally (always runs)
    arl> (try-catch (+ 1 2)
    arl>   (finally (display "cleanup")))    ; => 3
    #> "cleanup" 
    #> 3

    arl> ;; try with both catch and finally
    arl> (try-catch (stop "oops")
    arl>   (catch e (string-append "error: " ($ e "message")))
    arl>   (finally (display "done")))       ; => "error: oops"
    #> "done" 
    #> "error: oops"

    arl> ;; try with no handlers (just evaluates body)
    arl> (try-catch (+ 1 2))                       ; => 3
    #> 3

**Note:** Macro wrapper around `try` that provides familiar
catch/finally syntax. The catch clause binds the error condition to the
given variable. The finally clause runs regardless of success or
failure.

**See also:** [try](#try)

------------------------------------------------------------------------

## Continuations

### call-cc

Invoke receiver with the current continuation (escape procedure).

**Signature:** `(call-cc receiver)`

**Parameters:** - **`receiver`** — Function that receives the current
continuation as its argument

**Examples:**

    arl> (call-cc (lambda (k) (k 42)))  ; => 42
    #> 42

**Note:** Alias for R’s `callCC`. The receiver is passed the current
continuation.

**See also:**
[call-with-current-continuation](#call-with-current-continuation)

------------------------------------------------------------------------

### call-with-current-continuation

Full name alias for call-cc.

**Signature:** `(call-with-current-continuation receiver)`

**Parameters:** - **`receiver`** — Function that receives the current
continuation as its argument

**Examples:**

    arl> (call-with-current-continuation (lambda (k) (k 42)))  ; => 42
    #> 42

**Note:** Full name alias for `call-cc`.

**See also:** [call-cc](#call-cc)

------------------------------------------------------------------------

## Binding Macros

### pattern-symbols

Collect all symbols from a destructuring pattern, ignoring dots.

**Signature:** `(pattern-symbols pattern)`

**Parameters:** - **`pattern`** — Nested list/symbol pattern to extract
symbol names from

**See also:** [destructuring-bind](#destructuring-bind)

------------------------------------------------------------------------

### destructuring-bind

Bind a destructuring pattern to a value, then evaluate body forms.

**Signature:** `(destructuring-bind pattern value body...)`

**Parameters:** - **`pattern`** — Destructuring pattern (possibly
nested, with . for rest) - **`value`** — Expression whose result is
destructured - **`body`** — Expressions evaluated with the pattern
bindings in scope

**Examples:**

    arl> (destructuring-bind (a b c) (list 1 2 3)
    arl>   (+ a b c))                 ; => 6
    #> 6
    arl> (destructuring-bind (x . rest) (list 10 20 30)
    arl>   rest)                      ; => (20 30)
    #> (20 30)

**See also:** [let](#let), [let\*](#let-star),
[pattern-symbols](#pattern-symbols)

------------------------------------------------------------------------

### let

Bind names to values within body.

**Signature:** `(let bindings body...)`

**Parameters:** - **`bindings`** — List of (pattern value) pairs,
evaluated in parallel - **`body`** — Expressions evaluated with the
bindings in scope

**Examples:**

    arl> (let ((x 1) (y 2))
    arl>   (+ x y))               ; => 3
    #> 3

    arl> (let ((x 10))
    arl>   (* x x))               ; => 100
    #> 100

**Note:** Bindings are evaluated in parallel: earlier bindings are NOT
visible to later ones. Use `let*` for sequential binding.

**See also:** [let\*](#let-star), [letrec](#letrec)

------------------------------------------------------------------------

### let\*

Sequential let bindings.

**Parameters:** - **`bindings`** — List of (pattern value) pairs,
evaluated sequentially - **`body`** — Expressions evaluated with the
bindings in scope

**Examples:**

    arl> (let* ((x 1) (y (+ x 1)))
    arl>   (+ x y))               ; => 3
    #> 3

    arl> (let* ((a 10) (b (* a 2)) (c (+ a b)))
    arl>   c)                      ; => 30
    #> 30

**Note:** Bindings are evaluated sequentially: each binding can refer to
previously bound names. Supports destructuring patterns.

**See also:** [let](#let), [letrec](#letrec)

------------------------------------------------------------------------

### letrec

Recursive bindings.

**Signature:** `(letrec bindings body...)`

**Parameters:** - **`bindings`** — List of (name value) pairs, all
mutually visible - **`body`** — Expressions evaluated with the bindings
in scope

**Examples:**

    arl> (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
    arl>          (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
    arl>   (even? 10))             ; => #t
    #> TRUE

**Note:** All bindings are visible to all init expressions, enabling
mutual recursion. Init values are assigned via `set!` into pre-allocated
slots.

**See also:** [let](#let), [let\*](#let-star)

------------------------------------------------------------------------

### when-let

Bind pattern to value and evaluate body when value is truthy.

**Signature:** `(when-let binding body...)`

**Parameters:** - **`binding`** — Single (pattern value) pair -
**`body`** — Expressions evaluated when value is truthy, with pattern
bound

**Examples:**

    arl> (when-let (x (assoc 'a (list (list 'a 1) (list 'b 2))))
    arl>   (cadr x))              ; => 1
    #> 1

    arl> (when-let (x #f)
    arl>   "never reached")       ; => #nil

**See also:** [if-let](#if-let), [when](#when), [let](#let)

------------------------------------------------------------------------

### if-let

Bind pattern to value and choose branch based on its truthiness.

**Signature:** `(if-let binding then rest...)`

**Parameters:** - **`binding`** — Single (pattern value) pair -
**`then`** — Expression evaluated when value is truthy, with pattern
bound - **`rest`** — Optional else expression when value is falsy

**Examples:**

    arl> (if-let (x 42)
    arl>   (+ x 1)
    arl>   "nothing")             ; => 43
    #> 43

    arl> (if-let (x #f)
    arl>   "truthy"
    arl>   "falsy")               ; => "falsy"
    #> "falsy"

**See also:** [when-let](#when-let),
[if](https://willbrannon.com/arl/articles/lang-core.html#if),
[let](#let)

------------------------------------------------------------------------

## Looping Macros

### do-list

Loop over seq binding var each iteration. Expands to while.

**Signature:** `(do-list binding body...)`

**Parameters:** - **`binding`** — Pair of (var sequence-expr) -
**`body`** — Expressions evaluated for each element with var bound

**Examples:**

    arl> (define total 0)
    #> 0
    arl> (do-list (x '(1 2 3))
    arl>   (set! total (+ total x)))
    arl> total                       ; => 6
    #> 6

    arl> (define result (list))
    #> ()
    arl> (do-list (ch '("a" "b" "c"))
    arl>   (set! result (append result (list ch))))
    arl> result                      ; => ("a" "b" "c")
    #> ("a" "b" "c")

**Note:** Expands to a `while` loop over the sequence converted to a
list.

**See also:**
[while](https://willbrannon.com/arl/articles/lang-core.html#while),
[map](https://willbrannon.com/arl/articles/lang-functional.html#map),
[loop](#loop)

------------------------------------------------------------------------

### recur

Signal a recur back to the enclosing `loop`. Only valid inside a `loop`
body.

**Signature:** `(recur args...)`

**Parameters:** - **`args`** — New values for each loop variable (must
match loop binding count)

**Examples:**

    arl> (loop ((i 5) (acc 0))
    arl>   (if (= i 0) acc (recur (- i 1) (+ acc i))))  ; => 15
    #> 15

**See also:** [loop](#loop)

------------------------------------------------------------------------

### loop

Clojure-style loop/recur that expands to an R while loop.

**Signature:** `(loop bindings body...)`

**Parameters:** - **`bindings`** — List of (var init-value) pairs for
loop variables - **`body`** — Loop body; use (recur …) to iterate with
new values

**Examples:**

    arl> ;; Factorial via loop/recur
    arl> (loop ((n 5) (acc 1))
    arl>   (if (= n 0)
    arl>     acc
    arl>     (recur (- n 1) (* acc n))))  ; => 120
    #> 120

    arl> ;; Sum of a list via loop/recur
    arl> (loop ((xs '(1 2 3 4)) (total 0))
    arl>   (if (null? xs)
    arl>     total
    arl>     (recur (cdr xs) (+ total (car xs)))))  ; => 10
    #> 10

**Note:** The compiler auto-optimizes self-tail-calls in
`(define name (lambda ...))` patterns, so `loop`/`recur` is primarily
useful for anonymous iteration and mutual recursion patterns. Expands to
an R `while` loop internally.

**See also:** [recur](#recur),
[while](https://willbrannon.com/arl/articles/lang-core.html#while),
[do-list](#do-list)

------------------------------------------------------------------------

### until

Repeat body until test is truthy.

**Signature:** `(until test body...)`

**Parameters:** - **`test`** — Condition checked before each iteration;
loop stops when truthy - **`body`** — Expressions evaluated each
iteration

**Examples:**

    arl> (define i 0)
    #> 0
    arl> (until (= i 5)
    arl>   (set! i (+ i 1)))
    arl> i                          ; => 5
    #> 5

**See also:**
[while](https://willbrannon.com/arl/articles/lang-core.html#while)

------------------------------------------------------------------------

## Threading Macros

### -\>

Thread value through forms (first argument).

**Signature:** `(-> value forms...)`

**Parameters:** - **`value`** — Initial value to thread - **`forms`** —
Sequence of forms to thread through

**Examples:**

    arl> (-> 5 (+ 3) (* 2))       ; => 16  ; (5+3)*2
    #> 16
    arl> (-> (list 1 2 3) reverse) ; => (3 2 1)
    #> (3 2 1)
    arl> (-> "hello"
    arl>     nchar)                ; => 5
    #> 5

**Note:** Threads the value as the FIRST argument to each successive
form. If a form is a bare symbol, it is called as a one-argument
function.

**See also:** [-\>\>](#thread-last)

------------------------------------------------------------------------

### -\>\>

Thread value through forms (last argument).

**Signature:** `(->> value forms...)`

**Parameters:** - **`value`** — Initial value to thread - **`forms`** —
Sequence of forms to thread through

**Examples:**

    arl> (->> '(1 2 3) (map (lambda (x) (* x x))))  ; => (1 4 9)
    #> (1 4 9)
    arl> (->> 5 (- 10))           ; => 5  ; (- 10 5)
    #> 5
    arl> (->> '(1 2 3)
    arl>      (map (lambda (x) (+ x 10)))
    arl>      (car))              ; => 11
    #> 11

**Note:** Threads the value as the LAST argument to each successive
form. Compare with `->` which threads as the first argument.

**See also:** [-\>](#thread-first)

------------------------------------------------------------------------

### as-\>

Thread with a named binding. The value is bound to name in each form, so
it can appear in any position.

**Signature:** `(as-> expr name forms...)`

**Parameters:** - **`expr`** — Initial value - **`name`** — Symbol to
bind the threaded value to - **`forms`** — Sequence of forms using name

**Examples:**

    arl> (as-> 1 x (+ x 1) (* x 3))  ; => 6
    #> 6

**See also:** [-\>](#thread-first), [-\>\>](#thread-last)

------------------------------------------------------------------------

### some-\>

Thread-first with short-circuit on falsy values. Like -\> but stops and
returns \#nil or \#f if any step produces a falsy value.

**Signature:** `(some-> value forms...)`

**Parameters:** - **`value`** — Initial value - **`forms`** — Sequence
of forms to thread through

**Examples:**

    arl> (some-> 5 (+ 3) (* 2))   ; => 16
    #> 16
    arl> (some-> #nil (+ 3))       ; => #nil

**See also:** [-\>](#thread-first), [some-\>\>](#some-to-gt)

------------------------------------------------------------------------

### some-\>\>

Thread-last with short-circuit on falsy values. Like -\>\> but stops and
returns \#nil or \#f if any step produces a falsy value.

**Signature:** `(some->> value forms...)`

**Parameters:** - **`value`** — Initial value - **`forms`** — Sequence
of forms to thread through

**Examples:**

    arl> (some->> 5 (- 10))    ; => 5
    #> 5
    arl> (some->> #nil (- 10))  ; => #nil

**See also:** [-\>\>](#thread-last), [some-\>](#some-to)

------------------------------------------------------------------------

### cond-\>

Conditionally thread value through forms (first argument). Each clause
is (test form). If test is truthy, threads value through form; otherwise
passes value unchanged.

**Signature:** `(cond-> value clauses...)`

**Parameters:** - **`value`** — Initial value - **`clauses`** — Pairs of
(test form) to conditionally apply

**Examples:**

    arl> (cond-> 1 (#t (+ 1)) (#f (* 3)))  ; => 2
    #> 2

**See also:** [-\>](#thread-first), [cond-\>\>](#cond-to-gt)

------------------------------------------------------------------------

### cond-\>\>

Conditionally thread value through forms (last argument). Thread-last
variant of cond-\>.

**Signature:** `(cond->> value clauses...)`

**Parameters:** - **`value`** — Initial value - **`clauses`** — Pairs of
(test form) to conditionally apply

**Examples:**

    arl> (cond->> 5 (#t (- 10)) (#f (* 100)))  ; => 5
    #> 5

**See also:** [-\>\>](#thread-last), [cond-\>](#cond-to)

------------------------------------------------------------------------
