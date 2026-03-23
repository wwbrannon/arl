# Examples

These examples are complete Arl programs from
[`inst/examples`](https://github.com/wwbrannon/arl/tree/main/inst/examples/).
Each section pulls in the full source from the corresponding file, with
syntax highlighting, and links back to the repository version.

## Fibonacci

Recursive, iterative, and sequence-based Fibonacci implementations.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/fibonacci.arl>

    arl> ;; Fibonacci Sequence Implementations
    arl> ;; Demonstrates recursion, iteration, and functional approaches
    arl> ;;
    arl> ;; NOTE: Arl's compiler implements self-tail-call optimization (self-TCO).
    arl> ;; The `generate-next` helper inside `fib-sequence` benefits from this:
    arl> ;; `letrec` expands to `set!`, and the compiler applies self-TCO to
    arl> ;; `(set! name (lambda ...))` just as it does for `(define name (lambda ...))`,
    arl> ;; so the self-call in tail position is rewritten as a loop.
    arl> ;;
    arl> ;; The classic `fib-recursive` is NOT tail-recursive (it's doubly recursive),
    arl> ;; so TCO does not apply there. For large inputs, prefer `fib-iterative`
    arl> ;; or the sequence-based approach.

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> ;; ============================================================================
    arl> ;; Classic Recursive Implementation
    arl> ;; ============================================================================

    arl> (define fib-recursive
    arl>   (lambda (n)
    arl>     (cond
    arl>       ((= n 0) 0)
    arl>       ((= n 1) 1)
    arl>       (else (+ (fib-recursive (- n 1))
    arl>                (fib-recursive (- n 2)))))))
    #> <function>

    arl> (println "Recursive Fibonacci:")
    #> "Recursive Fibonacci:" 
    arl> (assert-equal 0 (fib-recursive 0))
    #> TRUE
    arl> (println (string-concat "fib(0) = " (fib-recursive 0)))
    #> "fib(0) = 0" 
    arl> (assert-equal 1 (fib-recursive 1))
    #> TRUE
    arl> (println (string-concat "fib(1) = " (fib-recursive 1)))
    #> "fib(1) = 1" 
    arl> (assert-equal 5 (fib-recursive 5))
    #> TRUE
    arl> (println (string-concat "fib(5) = " (fib-recursive 5)))
    #> "fib(5) = 5" 
    arl> (assert-equal 55 (fib-recursive 10))
    #> TRUE
    arl> (println (string-concat "fib(10) = " (fib-recursive 10)))
    #> "fib(10) = 55" 

    arl> ;; ============================================================================
    arl> ;; Iterative Implementation with While Loop
    arl> ;; ============================================================================

    arl> (define fib-iterative
    arl>   (lambda (n)
    arl>     (if (< n 2)
    arl>         n
    arl>         (begin
    arl>           (define a 0)
    arl>           (define b 1)
    arl>           (define i 2)
    arl>           (while (<= i n)
    arl>             (define temp b)
    arl>             (set! b (+ a b))
    arl>             (set! a temp)
    arl>             (set! i (+ i 1)))
    arl>           b))))
    #> <function>

    arl> (println "\nIterative Fibonacci:")
    #> "
    #> Iterative Fibonacci:" 
    arl> (assert-equal 55 (fib-iterative 10))
    #> TRUE
    arl> (println (string-concat "fib(10) = " (fib-iterative 10)))
    #> "fib(10) = 55" 
    arl> (assert-equal 610 (fib-iterative 15))
    #> TRUE
    arl> (println (string-concat "fib(15) = " (fib-iterative 15)))
    #> "fib(15) = 610" 

    arl> ;; ============================================================================
    arl> ;; Sequence Generation using unfold-like pattern
    arl> ;; ============================================================================

    arl> (define fib-sequence
    arl>   (lambda (n)
    arl>     (if (< n 1)
    arl>         ()
    arl>         (if (= n 1)
    arl>             (list 0)
    arl>             (if (= n 2)
    arl>                 (list 0 1)
    arl>                 (let* ((init (list 0 1)))
    arl>                   (letrec ((generate-next
    arl>                             (lambda (lst count)
    arl>                               (if (>= count n)
    arl>                                   lst
    arl>                                   (let* ((len (length lst))
    arl>                                          (a (nth lst (- len 2)))
    arl>                                          (b (nth lst (- len 1)))
    arl>                                          (next (+ a b)))
    arl>                                     (generate-next (append lst (list next))
    arl>                                                   (+ count 1)))))))
    arl>                     (generate-next init 2))))))))
    #> <function>

    arl> (println "\nFibonacci Sequence:")
    #> "
    #> Fibonacci Sequence:" 
    arl> (assert-equal (list 0 1 1 2 3 5 8 13 21 34) (fib-sequence 10))
    #> TRUE
    arl> (println (string-concat "First 10: " (fib-sequence 10)))
    #> "First 10: (0 1 1 2 3 5 8 13 21 34)" 
    arl> (assert-equal (list 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377) (fib-sequence 15))
    #> TRUE
    arl> (println (string-concat "First 15: " (fib-sequence 15)))
    #> "First 15: (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377)" 

    arl> ;; ============================================================================
    arl> ;; Using reduce to sum fibonacci sequence
    arl> ;; ============================================================================

    arl> (define sum-fib
    arl>   (lambda (n)
    arl>     (reduce + (fib-sequence n))))
    #> <function>

    arl> (println "\nSum of Fibonacci:")
    #> "
    #> Sum of Fibonacci:" 
    arl> (assert-equal 88 (sum-fib 10))
    #> TRUE
    arl> (println (string-concat "Sum of first 10: " (sum-fib 10)))
    #> "Sum of first 10: 88" 

    arl> ;; ============================================================================
    arl> ;; Memoized Fibonacci (using R environment for cache)
    arl> ;; ============================================================================

    arl> (define make-memoized-fib
    arl>   (lambda ()
    arl>     (let ((cache (new.env)))
    arl>       (lambda (n)
    arl>         (if (< n 2)
    arl>             n
    arl>             (let ((cached (get (string-concat n) cache)))
    arl>               (if (null? cached)
    arl>                   (let ((result (+ (fib-recursive (- n 1))
    arl>                                   (fib-recursive (- n 2)))))
    arl>                     (assign (string-concat n) result cache)
    arl>                     result)
    arl>                   cached)))))))
    #> <function>

    arl> ;; Note: Full memoization would require modifying recursive calls to use cache
    arl> ;; This is a simplified demonstration of the concept

    arl> (define fib10 (fib-recursive 10))
    #> 55
    arl> (define fib15 (fib-iterative 15))
    #> 610
    arl> (define seq10 (fib-sequence 10))
    #> (0 1 1 2 3 5 8 13 21 34)
    arl> (define sum10 (sum-fib 10))
    #> 88
    arl> (assert-equal 55 fib10)
    #> TRUE
    arl> (assert-equal 610 fib15)
    #> TRUE
    arl> (assert-equal (list 0 1 1 2 3 5 8 13 21 34) seq10)
    #> TRUE
    arl> (assert-equal 88 sum10)
    #> TRUE

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Quicksort

Quicksort and mergesort, with list operations and helpers.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/quicksort.arl>

    arl> ;; Quicksort Implementation
    arl> ;; Demonstrates list operations, filtering, and recursive algorithms

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> ;; ============================================================================
    arl> ;; Classic Quicksort
    arl> ;; ============================================================================

    arl> (define quicksort
    arl>   (lambda (lst)
    arl>     (if (null? lst)
    arl>         (list)
    arl>         (let* ((pivot (car lst))
    arl>                (rest-lst (cdr lst))
    arl>                (smaller (filter (lambda (x) (< x pivot)) rest-lst))
    arl>                (greater (filter (lambda (x) (>= x pivot)) rest-lst)))
    arl>           (append (append (quicksort smaller)
    arl>                          (list pivot))
    arl>                   (quicksort greater))))))
    #> <function>

    arl> (println "Quicksort Examples:")
    #> "Quicksort Examples:" 
    arl> (assert-equal (list 1 1 2 3 4 5 6 9) (quicksort (list 3 1 4 1 5 9 2 6)))
    #> TRUE
    arl> (println (string-concat "Sort [3 1 4 1 5 9 2 6]: " (quicksort (list 3 1 4 1 5 9 2 6))))
    #> "Sort [3 1 4 1 5 9 2 6]: (1 1 2 3 4 5 6 9)" 
    arl> (assert-equal (list 1 2 3 4 5) (quicksort (list 5 4 3 2 1)))
    #> TRUE
    arl> (println (string-concat "Sort [5 4 3 2 1]: " (quicksort (list 5 4 3 2 1))))
    #> "Sort [5 4 3 2 1]: (1 2 3 4 5)" 
    arl> (assert-equal (list 1) (quicksort (list 1)))
    #> TRUE
    arl> (println (string-concat "Sort [1]: " (quicksort (list 1))))
    #> "Sort [1]: (1)" 
    arl> (assert-equal (list) (quicksort (list)))
    #> TRUE
    arl> (println (string-concat "Sort []: " (quicksort (list))))
    #> "Sort []: ()" 

    arl> ;; ============================================================================
    arl> ;; Three-way Partition Quicksort (handles duplicates better)
    arl> ;; ============================================================================

    arl> (define quicksort-3way
    arl>   (lambda (lst)
    arl>     (if (null? lst)
    arl>         (list)
    arl>         (let* ((pivot (car lst))
    arl>                (rest-lst (cdr lst))
    arl>                (smaller (filter (lambda (x) (< x pivot)) rest-lst))
    arl>                (equal (filter (lambda (x) (= x pivot)) rest-lst))
    arl>                (greater (filter (lambda (x) (> x pivot)) rest-lst)))
    arl>           (append (append (quicksort-3way smaller)
    arl>                           (cons pivot equal))
    arl>                   (quicksort-3way greater))))))
    #> <function>

    arl> (println "\nThree-way Quicksort:")
    #> "
    #> Three-way Quicksort:" 
    arl> (assert-equal (list 1 1 2 3 3 4 5 5 6 9) (quicksort-3way (list 3 1 4 1 5 9 2 6 5 3)))
    #> TRUE
    arl> (println (string-concat "Sort [3 1 4 1 5 9 2 6 5 3]: "
    arl>               (quicksort-3way (list 3 1 4 1 5 9 2 6 5 3))))
    #> "Sort [3 1 4 1 5 9 2 6 5 3]: (1 1 2 3 3 4 5 5 6 9)" 

    arl> ;; ============================================================================
    arl> ;; Using partition to split list
    arl> ;; ============================================================================

    arl> (define partition-by-pivot
    arl>   (lambda (pivot lst)
    arl>     (let ((smaller (filter (lambda (x) (< x pivot)) lst))
    arl>           (greater (filter (lambda (x) (>= x pivot)) lst)))
    arl>       (list smaller greater))))
    #> <function>

    arl> (println "\nPartition Example:")
    #> "
    #> Partition Example:" 
    arl> (let ((result (partition-by-pivot 5 (list 3 7 2 9 1 5 8 4))))
    arl>   (assert-equal (list 3 2 1 4) (car result))
    arl>   (assert-equal (list 7 9 5 8) (car (cdr result)))
    arl>   (println (string-concat "Partition by 5: smaller=" (car result)
    arl>                 " greater=" (car (cdr result)))))
    #> "Partition by 5: smaller=(3 2 1 4) greater=(7 9 5 8)" 

    arl> ;; ============================================================================
    arl> ;; Demonstrating list operations
    arl> ;; ============================================================================

    arl> (println "\nList Operations Demo:")
    #> "
    #> List Operations Demo:" 

    arl> ;; take and drop
    arl> (define data (list 10 20 30 40 50))
    #> (10 20 30 40 50)
    arl> (println (string-concat "Original: " data))
    #> "Original: (10 20 30 40 50)" 
    arl> (assert-equal (list 10 20 30) (take 3 data))
    #> TRUE
    arl> (println (string-concat "Take 3: " (take 3 data)))
    #> "Take 3: (10 20 30)" 
    arl> (assert-equal (list 40 50) (drop 3 data))
    #> TRUE
    arl> (println (string-concat "Drop 3: " (drop 3 data)))
    #> "Drop 3: (40 50)" 

    arl> ;; reverse
    arl> (assert-equal (list 50 40 30 20 10) (reverse data))
    #> TRUE
    arl> (println (string-concat "Reversed: " (reverse data)))
    #> "Reversed: (50 40 30 20 10)" 

    arl> ;; map to transform
    arl> (assert-equal (list 20 40 60 80 100) (map (lambda (x) (* x 2)) data))
    #> TRUE
    arl> (println (string-concat "Double all: " (map (lambda (x) (* x 2)) data)))
    #> "Double all: (20 40 60 80 100)" 

    arl> ;; reduce to aggregate
    arl> (assert-equal 150 (reduce + data))
    #> TRUE
    arl> (println (string-concat "Sum: " (reduce + data)))
    #> "Sum: 150" 
    arl> (assert-equal 12000000 (reduce * data))
    #> TRUE
    arl> (println (string-concat "Product: " (reduce * data)))
    #> "Product: 1.2e+07" 

    arl> ;; ============================================================================
    arl> ;; Merge Sort for comparison
    arl> ;; ============================================================================

    arl> (define merge
    arl>   (lambda (left right)
    arl>     (cond
    arl>       ((null? left) right)
    arl>       ((null? right) left)
    arl>       ((< (car left) (car right))
    arl>        (cons (car left) (merge (cdr left) right)))
    arl>       (else
    arl>        (cons (car right) (merge left (cdr right)))))))
    #> <function>

    arl> (define mergesort
    arl>   (lambda (lst)
    arl>     (if (<= (length lst) 1)
    arl>         lst
    arl>         (let* ((mid (/ (length lst) 2))
    arl>                (left (take mid lst))
    arl>                (right (drop mid lst)))
    arl>           (merge (mergesort left) (mergesort right))))))
    #> <function>

    arl> (println "\nMergesort Example:")
    #> "
    #> Mergesort Example:" 
    arl> (assert-equal (list 1 1 2 3 4 5 6 9) (mergesort (list 3 1 4 1 5 9 2 6)))
    #> TRUE
    arl> (println (string-concat "Sort [3 1 4 1 5 9 2 6]: " (mergesort (list 3 1 4 1 5 9 2 6))))
    #> "Sort [3 1 4 1 5 9 2 6]: (1 1 2 3 4 5 6 9)" 

    arl> (define sorted-small (quicksort (list 3 1 4)))
    #> (1 3 4)
    arl> (define merge-sorted (mergesort (list 3 1 4 1 5 9 2 6)))
    #> (1 1 2 3 4 5 6 9)
    arl> (assert-equal (list 1 3 4) sorted-small)
    #> TRUE
    arl> (assert-equal (list 1 1 2 3 4 5 6 9) merge-sorted)
    #> TRUE

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## FizzBuzz

Multiple FizzBuzz variations showing control flow and list processing.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/fizzbuzz.arl>

    arl> ;; FizzBuzz Implementations
    arl> ;; Demonstrates control flow, looping, and conditionals

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import math :refer :all)
    arl> (import threading :refer :all)

    arl> ;; ============================================================================
    arl> ;; Classic FizzBuzz using cond
    arl> ;; ============================================================================

    arl> (define fizzbuzz
    arl>   (lambda (n)
    arl>     (cond
    arl>       ((= (% n 15) 0) "FizzBuzz")
    arl>       ((= (% n 3) 0) "Fizz")
    arl>       ((= (% n 5) 0) "Buzz")
    arl>       (else (string-concat n)))))
    #> <function>

    arl> (assert-equal "1" (fizzbuzz 1))
    #> TRUE
    arl> (assert-equal "2" (fizzbuzz 2))
    #> TRUE
    arl> (assert-equal "Fizz" (fizzbuzz 3))
    #> TRUE
    arl> (assert-equal "Buzz" (fizzbuzz 5))
    #> TRUE
    arl> (assert-equal "FizzBuzz" (fizzbuzz 15))
    #> TRUE

    arl> (println "Classic FizzBuzz (1-20):")
    #> "Classic FizzBuzz (1-20):" 
    arl> (define i 1)
    #> 1
    arl> (while (<= i 20)
    arl>   (println (fizzbuzz i))
    arl>   (set! i (+ i 1)))
    #> "1" 
    #> "2" 
    #> "Fizz" 
    #> "4" 
    #> "Buzz" 
    #> "Fizz" 
    #> "7" 
    #> "8" 
    #> "Fizz" 
    #> "Buzz" 
    #> "11" 
    #> "Fizz" 
    #> "13" 
    #> "14" 
    #> "FizzBuzz" 
    #> "16" 
    #> "17" 
    #> "Fizz" 
    #> "19" 
    #> "Buzz" 

    arl> ;; ============================================================================
    arl> ;; Using for loop
    arl> ;; ============================================================================

    arl> (println "\nUsing for loop (1-15):")
    #> "
    #> Using for loop (1-15):" 
    arl> (do-list (n (seq 1 15))
    arl>   (println (fizzbuzz n)))
    #> "1" 
    #> "2" 
    #> "Fizz" 
    #> "4" 
    #> "Buzz" 
    #> "Fizz" 
    #> "7" 
    #> "8" 
    #> "Fizz" 
    #> "Buzz" 
    #> "11" 
    #> "Fizz" 
    #> "13" 
    #> "14" 
    #> "FizzBuzz" 

    arl> ;; ============================================================================
    arl> ;; Functional approach with map
    arl> ;; ============================================================================

    arl> (println "\nFunctional approach (1-10):")
    #> "
    #> Functional approach (1-10):" 
    arl> (define fizzbuzz-list
    arl>   (lambda (n)
    arl>     (map fizzbuzz (seq 1 n))))
    #> <function>

    arl> (assert-equal (list "1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz") (fizzbuzz-list 10))
    #> TRUE
    arl> (println (fizzbuzz-list 10))
    #> ("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz") 

    arl> ;; ============================================================================
    arl> ;; FizzBuzz with when/unless
    arl> ;; ============================================================================

    arl> (define fizzbuzz-when
    arl>   (lambda (n)
    arl>     (let ((result ""))
    arl>       (when (= (% n 3) 0)
    arl>         (set! result (string-concat result "Fizz")))
    arl>       (when (= (% n 5) 0)
    arl>         (set! result (string-concat result "Buzz")))
    arl>       (when (= result "")
    arl>         (set! result (string-concat n)))
    arl>       result)))
    #> <function>

    arl> (assert-equal "Fizz" (fizzbuzz-when 3))
    #> TRUE
    arl> (assert-equal "Buzz" (fizzbuzz-when 5))
    #> TRUE
    arl> (assert-equal "FizzBuzz" (fizzbuzz-when 15))
    #> TRUE
    arl> (assert-equal "1" (fizzbuzz-when 1))
    #> TRUE

    arl> (println "\nUsing when (1-10):")
    #> "
    #> Using when (1-10):" 
    arl> (do-list (n (seq 1 10))
    arl>   (println (fizzbuzz-when n)))
    #> "1" 
    #> "2" 
    #> "Fizz" 
    #> "4" 
    #> "Buzz" 
    #> "Fizz" 
    #> "7" 
    #> "8" 
    #> "Fizz" 
    #> "Buzz" 

    arl> ;; ============================================================================
    arl> ;; Case statement version
    arl> ;; ============================================================================

    arl> (define fizzbuzz-case
    arl>   (lambda (n)
    arl>     (case (% n 15)
    arl>       ((0) "FizzBuzz")
    arl>       ((3 6 9 12) "Fizz")
    arl>       ((5 10) "Buzz")
    arl>       (else (string-concat n)))))
    #> <function>

    arl> (println "\nUsing case (note: case checks exact value, not pattern):")
    #> "
    #> Using case (note: case checks exact value, not pattern):" 
    arl> (println "1-15 with case-based FizzBuzz:")
    #> "1-15 with case-based FizzBuzz:" 
    arl> (do-list (n (seq 1 15))
    arl>   (println (fizzbuzz-case n)))
    #> "1" 
    #> "2" 
    #> "Fizz" 
    #> "4" 
    #> "Buzz" 
    #> "Fizz" 
    #> "7" 
    #> "8" 
    #> "Fizz" 
    #> "Buzz" 
    #> "11" 
    #> "Fizz" 
    #> "13" 
    #> "14" 
    #> "FizzBuzz" 

    arl> ;; ============================================================================
    arl> ;; Filter to find Fizz, Buzz, and FizzBuzz numbers
    arl> ;; ============================================================================

    arl> (println "\nFiltering FizzBuzz numbers:")
    #> "
    #> Filtering FizzBuzz numbers:" 

    arl> (define range-1-30 (seq 1 30))
    #> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30

    arl> (define fizz-numbers
    arl>   (filter (lambda (n) (= (% n 3) 0)) range-1-30))
    #> (3 6 9 12 15 18 21 24 27 30)

    arl> (define buzz-numbers
    arl>   (filter (lambda (n) (= (% n 5) 0)) range-1-30))
    #> (5 10 15 20 25 30)

    arl> (define fizzbuzz-numbers
    arl>   (filter (lambda (n) (= (% n 15) 0)) range-1-30))
    #> (15 30)

    arl> (assert-equal (list 3 6 9 12 15 18 21 24 27 30) fizz-numbers)
    #> TRUE
    arl> (assert-equal (list 5 10 15 20 25 30) buzz-numbers)
    #> TRUE
    arl> (assert-equal (list 15 30) fizzbuzz-numbers)
    #> TRUE
    arl> (println (string-concat "Fizz numbers (1-30): " fizz-numbers))
    #> "Fizz numbers (1-30): (3 6 9 12 15 18 21 24 27 30)" 
    arl> (println (string-concat "Buzz numbers (1-30): " buzz-numbers))
    #> "Buzz numbers (1-30): (5 10 15 20 25 30)" 
    arl> (println (string-concat "FizzBuzz numbers (1-30): " fizzbuzz-numbers))
    #> "FizzBuzz numbers (1-30): (15 30)" 

    arl> ;; ============================================================================
    arl> ;; Count FizzBuzz occurrences
    arl> ;; ============================================================================

    arl> (define count-fizzbuzz
    arl>   (lambda (n)
    arl>     (let ((results (map fizzbuzz (seq 1 n))))
    arl>       (dict
    arl>        :fizz (length (filter (lambda (x) (= x "Fizz")) results))
    arl>        :buzz (length (filter (lambda (x) (= x "Buzz")) results))
    arl>        :fizzbuzz (length (filter (lambda (x) (= x "FizzBuzz")) results))
    arl>        :numbers (length (filter (lambda (x) (not (or (= x "Fizz")
    arl>                                                        (= x "Buzz")
    arl>                                                        (= x "FizzBuzz"))))
    arl>                                 results))))))
    #> <function>

    arl> (println "\nCounts for 1-100:")
    #> "
    #> Counts for 1-100:" 
    arl> (define counts-100 (count-fizzbuzz 100))
    #> 27 14 6 53
    arl> (assert-equal 27 (get "fizz" counts-100))
    #> TRUE
    arl> (assert-equal 14 (get "buzz" counts-100))
    #> TRUE
    arl> (assert-equal 6 (get "fizzbuzz" counts-100))
    #> TRUE
    arl> (assert-equal 53 (get "numbers" counts-100))
    #> TRUE
    arl> (println counts-100)
    #> 27 14 6 53 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Macro Examples

Macro definitions, quasiquote, expansion, and DSL-style helpers.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/macro-examples.arl>

    arl> ;; Macro System Examples
    arl> ;; Demonstrates defmacro, quasiquote, unquote, and macro expansion

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> ;; ============================================================================
    arl> ;; Simple Macros
    arl> ;; ============================================================================

    arl> (println "=== Simple Macros ===\n")
    #> "=== Simple Macros ===
    #> " 

    arl> ;; A macro that doubles its argument
    arl> ;;' @description Multiply x by 2 at macro expansion time.
    arl> (defmacro double (x)
    arl>   `(* 2 ,x))

    arl> (assert-equal 10 (double 5))
    #> TRUE
    arl> (println (string-concat "(double 5) = " (double 5)))
    #> "(double 5) = 10" 
    arl> (assert-equal 6 (double (+ 1 2)))
    #> TRUE
    arl> (println (string-concat "(double (+ 1 2)) = " (double (+ 1 2))))
    #> "(double (+ 1 2)) = 6" 

    arl> ;; A macro that creates a variable with a default value
    arl> ;;' @description Define a variable with a value.
    arl> (defmacro defvar (name value)
    arl>   `(define ,name ,value))

    arl> (defvar x 42)
    #> 42
    arl> (assert-equal 42 x)
    #> TRUE
    arl> (println (string-concat "x = " x))
    #> "x = 42" 

    arl> ;; ============================================================================
    arl> ;; Macros with Multiple Expressions
    arl> ;; ============================================================================

    arl> (println "\n=== Macros with Multiple Expressions ===\n")
    #> "
    #> === Macros with Multiple Expressions ===
    #> " 

    arl> ;; A macro that executes multiple forms and returns the last
    arl> ;;' @description Evaluate forms in order and return the last.
    arl> (defmacro progn (first . rest)
    arl>   `(begin ,first ,@rest))

    arl> (println "Using progn:")
    #> "Using progn:" 
    arl> (progn
    arl>   (println "First")
    arl>   (println "Second")
    arl>   (println "Third"))
    #> "First" 
    #> "Second" 
    #> "Third" 

    arl> ;; ============================================================================
    arl> ;; Control Flow Macros
    arl> ;; ============================================================================

    arl> (println "\n=== Control Flow Macros ===\n")
    #> "
    #> === Control Flow Macros ===
    #> " 

    arl> ;; Custom when-not macro
    arl> ;;' @description Evaluate body when test is falsy.
    arl> (defmacro when-not (test . body)
    arl>   `(if (not ,test)
    arl>        (begin ,@body)
    arl>        #nil))

    arl> (when-not #f
    arl>   (println "This should print"))
    #> "This should print" 

    arl> (when-not #t
    arl>   (println "This should not print"))

    arl> ;; A do-times macro
    arl> ;;' @description Repeat body n times.
    arl> (defmacro do-times (n . body)
    arl>   (let ((counter (gensym "counter")))
    arl>     `(let ((,counter 0))
    arl>        (while (< ,counter ,n)
    arl>          (begin ,@body)
    arl>          (set! ,counter (+ ,counter 1))))))

    arl> (println "do-times example:")
    #> "do-times example:" 
    arl> (define count 0)
    #> 0
    arl> (do-times 5
    arl>   (set! count (+ count 1))
    arl>   (println (string-concat "Count: " count)))
    #> "Count: 1" 
    #> "Count: 2" 
    #> "Count: 3" 
    #> "Count: 4" 
    #> "Count: 5" 
    arl> (assert-equal 5 count)
    #> TRUE

    arl> ;; ============================================================================
    arl> ;; Macro Expansion Examples
    arl> ;; ============================================================================

    arl> (println "\n=== Macro Expansion ===\n")
    #> "
    #> === Macro Expansion ===
    #> " 

    arl> (println "Original: (when (> 5 3) (println \"yes\"))")
    #> "Original: (when (> 5 3) (println "yes"))" 
    arl> (println "Expanded:")
    #> "Expanded:" 
    arl> (println (macroexpand-1 '(when (> 5 3) (println "yes"))))
    #> (if (> 5 3) (begin (println "yes")) .__nil) 

    arl> (println "\nOriginal: (let ((x 1)) (+ x 2))")
    #> "
    #> Original: (let ((x 1)) (+ x 2))" 
    arl> (println "Expanded:")
    #> "Expanded:" 
    arl> (println (macroexpand-1 '(let ((x 1)) (+ x 2))))
    #> ((lambda (tmp__268) (define x tmp__268) (begin (+ x 2))) 1) 

    arl> ;; ============================================================================
    arl> ;; Anaphoric Macros (macros that capture a name)
    arl> ;; ============================================================================

    arl> (println "\n=== Anaphoric Macros ===\n")
    #> "
    #> === Anaphoric Macros ===
    #> " 

    arl> ;; An anaphoric if that binds result to 'it'
    arl> ;;' @description Bind test result to it and select then/alt.
    arl> (defmacro aif (test then alt)
    arl>   `(let ((it ,test))
    arl>      (if it ,(capture 'it then) ,(capture 'it alt))))

    arl> (println "Anaphoric if example:")
    #> "Anaphoric if example:" 
    arl> (aif (+ 2 3)
    arl>      (println (string-concat "Result is: " it))
    arl>      (println "No result"))
    #> "Result is: 5" 

    arl> ;; ============================================================================
    arl> ;; Building DSLs with Macros
    arl> ;; ============================================================================

    arl> (println "\n=== Simple DSL Example ===\n")
    #> "
    #> === Simple DSL Example ===
    #> " 

    arl> ;; A macro for defining test cases
    arl> ;;' @description Define and run a simple test case.
    arl> (defmacro deftest (name . body)
    arl>   `(begin
    arl>      (println (string-concat "Running test: " (quote ,name)))
    arl>      ,@body
    arl>      (println "Test passed!\n")))

    arl> (deftest addition-works
    arl>   (assert (= (+ 1 2) 3) "1 + 2 should equal 3")
    arl>   (assert (= (+ 5 5) 10) "5 + 5 should equal 10"))
    #> "Running test: addition-works" 
    #> "Test passed!
    #> " 

    arl> (deftest string-operations
    arl>   (assert (= (string-concat "hello" " " "world") "hello world")
    arl>           "String concatenation works"))
    #> "Running test: string-operations" 
    #> "Test passed!
    #> " 

    arl> ;; ============================================================================
    arl> ;; Hygiene and explicit capture
    arl> ;; ============================================================================

    arl> (println "\n=== Hygiene and Capture ===\n")
    #> "
    #> === Hygiene and Capture ===
    #> " 

    arl> ;; Automatic hygiene (no gensym needed)
    arl> ;;' @description Swap two bindings using a temporary.
    arl> (defmacro swap (a b)
    arl>   `(let ((temp ,a))
    arl>      (set! ,a ,b)
    arl>      (set! ,b temp)))

    arl> (println "Demonstrating gensym:")
    #> "Demonstrating gensym:" 
    arl> (println (string-concat "Generated symbol 1: " (gensym)))
    #> "Generated symbol 1: G__274" 
    arl> (println (string-concat "Generated symbol 2: " (gensym)))
    #> "Generated symbol 2: G__275" 
    arl> (println (string-concat "Generated symbol with prefix: " (gensym "my-prefix")))
    #> "Generated symbol with prefix: my-prefix__276" 

    arl> ;; ============================================================================
    arl> ;; Quasiquote Examples
    arl> ;; ============================================================================

    arl> (println "\n=== Quasiquote Examples ===\n")
    #> "
    #> === Quasiquote Examples ===
    #> " 

    arl> (define x 10)
    #> 10
    arl> (define y 20)
    #> 20

    arl> (println "Without quasiquote:")
    #> "Without quasiquote:" 
    arl> (println (list '+ 1 2))
    #> (+ 1 2) 

    arl> (println "\nWith quasiquote and unquote:")
    #> "
    #> With quasiquote and unquote:" 
    arl> (println `(+ ,x ,y))
    #> (+ 10 20) 

    arl> (println "\nWith unquote-splicing:")
    #> "
    #> With unquote-splicing:" 
    arl> (define nums (list 1 2 3))
    #> (1 2 3)
    arl> (println `(+ ,@nums))  ; Should expand to (+ 1 2 3)
    #> (+ 1 2 3) 

    arl> ;; ============================================================================
    arl> ;; Recursive Macros
    arl> ;; ============================================================================

    arl> (println "\n=== Recursive Macro Example ===\n")
    #> "
    #> === Recursive Macro Example ===
    #> " 

    arl> ;; A macro that chains function calls
    arl> ;;' @description Thread value through forms (first argument).
    arl> (defmacro -> (value . forms)
    arl>   (if (null? forms)
    arl>       value
    arl>       (let ((first-form (car forms))
    arl>             (rest-forms (cdr forms)))
    arl>         (if (list-or-pair? first-form)
    arl>             `(-> (,(car first-form) ,value ,@(cdr first-form)) ,@rest-forms)
    arl>             `(-> (,first-form ,value) ,@rest-forms)))))

    arl> (println "Threading macro example:")
    #> "Threading macro example:" 
    arl> (assert-equal 15 (-> 5
    arl>                      (+ 3)
    arl>                      (* 2)
    arl>                      (- 1)))
    #> TRUE
    arl> (println (string-concat "Result: " (-> 5
    arl>                               (+ 3)
    arl>                               (* 2)
    arl>                               (- 1))))  ; ((5 + 3) * 2) - 1 = 15
    #> "Result: 15" 

    arl> ;; ============================================================================
    arl> ;; Compile-Time Computation
    arl> ;; ============================================================================

    arl> (println "\n=== Compile-Time Computation ===\n")
    #> "
    #> === Compile-Time Computation ===
    #> " 

    arl> ;; A macro that computes at macro expansion time
    arl> ;;' @description Compute the product of a and b at expand time.
    arl> (defmacro const-multiply (a b)
    arl>   (let ((result (* a b)))
    arl>     `(quote ,result)))

    arl> (assert-equal 42 (const-multiply 6 7))
    #> TRUE
    arl> (println (string-concat "(const-multiply 6 7) = " (const-multiply 6 7)))
    #> "(const-multiply 6 7) = 42" 
    arl> (println "This multiplication happened at macro expansion time!")
    #> "This multiplication happened at macro expansion time!" 

    arl> (define threading-result (-> 5 (+ 3) (* 2) (- 1)))
    #> 15
    arl> (define aif-result (aif (+ 2 3) it #nil))
    #> 5
    arl> (assert-equal 15 threading-result)
    #> TRUE
    arl> (assert-equal 5 aif-result)
    #> TRUE

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Pipeline Macros

Threading a value through a custom pipeline macro.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/pipeline-macros.arl>

    arl> ;; Pipeline Macros Example
    arl> ;; Demonstrates a small macro-driven data pipeline

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> ;; Pipeline macro: threads a value through a sequence of steps
    arl> ;;' @description Thread value through steps (last argument).
    arl> (defmacro pipeline (value . steps)
    arl>   (if (null? steps)
    arl>       value
    arl>       (let ((step (car steps))
    arl>             (rest-steps (cdr steps)))
    arl>         (if (list-or-pair? step)
    arl>             `(pipeline (,(car step) ,@(cdr step) ,value) ,@rest-steps)
    arl>             `(pipeline (,step ,value) ,@rest-steps)))))

    arl> (define data (list 1 2 3 4 5 6 7 8 9))
    #> (1 2 3 4 5 6 7 8 9)

    arl> (define pipeline-result
    arl>   (pipeline data
    arl>             (filter (lambda (x) (> x 0)))
    arl>             (map (lambda (x) x))
    arl>             (reduce +)))
    #> 45

    arl> (println "=== Pipeline Output ===")
    #> "=== Pipeline Output ===" 
    arl> (assert-equal (list 1 2 3 4 5 6 7 8 9) data)
    #> TRUE
    arl> (println (string-concat "Input: " data))
    #> "Input: (1 2 3 4 5 6 7 8 9)" 
    arl> (assert-equal 45 pipeline-result)
    #> TRUE
    arl> (println (string-concat "Pipeline result: " pipeline-result))
    #> "Pipeline result: 45" 

    arl> (define expanded (macroexpand-1
    arl>                   '(pipeline data
    arl>                              (filter (lambda (x) (> x 0)))
    arl>                              (map (lambda (x) x))
    arl>                              (reduce +))))
    #> (pipeline (filter (lambda (x) (> x 0)) data) (map (lambda (x) x)) (reduce +))

    arl> (println "\nExpanded form:")
    #> "
    #> Expanded form:" 
    arl> (println expanded)
    #> (pipeline (filter (lambda (x) (> x 0)) data) (map (lambda (x) x)) (reduce +)) 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Data Analysis

R interoperability and data transformations.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/data-analysis.arl>

    arl> ;; Data Analysis with R Interop
    arl> ;; Demonstrates seamless integration between Arl and R
    arl> ;;
    arl> ;; NOTE ON R INTEROP: All R functions are directly callable from Arl:
    arl> ;;
    arl> ;;   (mean (c 1 2 3))              ; call R's mean() and c()
    arl> ;;   (seq :from 1 :to 10 :by 2)   ; named arguments via keywords
    arl> ;;
    arl> ;; Arl also provides r-call, which bypasses bindings in intermediate
    arl> ;; scopes to reach objects in R environments that Arl's stack isn't
    arl> ;; managing. For example:
    arl> ;;
    arl> ;;   (r-call "mean" (list (c 1 2 3)))         ; look up "mean" by string name
    arl> ;;   (r-call "fn" args (some-environment))    ; search a specific environment
    arl> ;;
    arl> ;; In practice, direct calls are simpler and preferred. This example
    arl> ;; uses direct calls throughout.

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> ;; Deterministic randomness for reproducible examples
    arl> (set.seed 123)

    arl> ;; ============================================================================
    arl> ;; Working with R Vectors and Lists
    arl> ;; ============================================================================

    arl> (println "=== R Interop Basics ===\n")
    #> "=== R Interop Basics ===
    #> " 

    arl> ;; Create R vectors directly
    arl> (define nums (c 1 2 3 4 5 6 7 8 9 10))
    #> 1 2 3 4 5 6 7 8 9 10
    arl> (assert-equal (list 1 2 3 4 5 6 7 8 9 10) (as.list nums))
    #> TRUE
    arl> (println (string-concat "R vector: " nums))
    #> "R vector: 1 2 3 4 5 6 7 8 9 10" 

    arl> ;; Use R statistical functions
    arl> (define mean-val (mean nums))
    #> 5.5
    arl> (define median-val ((:: stats median) nums))
    #> 5.5
    arl> (define sd-val ((:: stats sd) nums))
    #> 3.02765035409749
    arl> (assert-equal 5.5 mean-val)
    #> TRUE
    arl> (assert-equal 5.5 median-val)
    #> TRUE
    arl> (assert-true (> sd-val 3.0))
    #> TRUE
    arl> (assert-true (< sd-val 3.1))
    #> TRUE

    arl> (println (string-concat "Mean: " mean-val))
    #> "Mean: 5.5" 
    arl> (println (string-concat "Median: " median-val))
    #> "Median: 5.5" 
    arl> (println (string-concat "SD: " sd-val))
    #> "SD: 3.02765035409749" 

    arl> ;; ============================================================================
    arl> ;; Generating Data with R
    arl> ;; ============================================================================

    arl> (println "\n=== Generating Data ===\n")
    #> "
    #> === Generating Data ===
    #> " 

    arl> ;; Generate random normal data
    arl> (define random-data ((:: stats rnorm) 100))
    #> -0.560475646552213 -0.23017748948328 1.55870831414912 0.070508391424576 0.129287735160946 1.71506498688328 0.460916205989202 -1.26506123460653 -0.686852851893526 -0.445661970099958 1.22408179743946 0.359813827057364 0.400771450594052 0.11068271594512 -0.555841134754075 1.78691313680308 0.497850478229239 -1.96661715662964 0.701355901563686 -0.472791407727934 -1.06782370598685 -0.217974914658295 -1.02600444830724 -0.72889122929114 -0.625039267849257 -1.68669331074241 0.837787044494525 0.153373117836515 -1.13813693701195 1.25381492106993 0.426464221476814 -0.295071482992271 0.895125661045022 0.878133487533042 0.821581081637487 0.688640254100091 0.553917653537589 -0.0619117105767217 -0.305962663739917 -0.380471001012383 -0.694706978920513 -0.207917278019599 -1.26539635156826 2.16895596533851 1.20796199830499 -1.12310858320335 -0.402884835299076 -0.466655353623219 0.779965118336318 -0.0833690664718293 0.253318513994755 -0.028546755348703 -0.0428704572913161 1.36860228401446 -0.225770985659268 1.51647060442954 -1.54875280423022 0.584613749636069 0.123854243844614 0.215941568743973 0.379639482759882 -0.502323453109302 -0.33320738366942 -1.01857538310709 -1.07179122647558 0.303528641404258 0.448209778629426 0.0530042267305041 0.922267467879738 2.05008468562714 -0.491031166056535 -2.30916887564081 1.00573852446226 -0.709200762582393 -0.688008616467358 1.0255713696967 -0.284773007051009 -1.22071771225454 0.18130347974915 -0.138891362439045 0.00576418589988693 0.38528040112633 -0.370660031792409 0.644376548518833 -0.220486561818751 0.331781963915697 1.09683901314935 0.435181490833803 -0.325931585531227 1.14880761845109 0.993503855962119 0.54839695950807 0.238731735111441 -0.627906076039371 1.36065244853001 -0.600259587147127 2.18733299301658 1.53261062618519 -0.235700359100477 -1.02642090030678
    arl> (println (string-concat "Generated 100 random numbers"))
    #> "Generated 100 random numbers" 
    arl> (println (string-concat "First 10: " ((:: utils head) random-data 10)))
    #> "First 10: -0.560475646552213 -0.23017748948328 1.55870831414912 0.070508391424576 0.129287735160946 1.71506498688328 0.460916205989202 -1.26506123460653 -0.686852851893526 -0.445661970099958" 

    arl> ;; Summary statistics
    arl> (println "\nSummary:")
    #> "
    #> Summary:" 
    arl> (println (summary random-data))
    #> (:Min. -2.30916887564081 :1st Qu. -0.493854237819727 :Median 0.0617563090775401 :Mean 0.0904059086362066 :3rd Qu. 0.69181916596599 :Max. 2.18733299301658) 

    arl> ;; ============================================================================
    arl> ;; Data Transformation Pipeline
    arl> ;; ============================================================================

    arl> (println "\n=== Data Transformation Pipeline ===\n")
    #> "
    #> === Data Transformation Pipeline ===
    #> " 

    arl> ;; Create sample data
    arl> (define scores (c 85 92 78 90 88 76 95 89 91 87))
    #> 85 92 78 90 88 76 95 89 91 87
    arl> (println (string-concat "Original scores: " scores))
    #> "Original scores: 85 92 78 90 88 76 95 89 91 87" 

    arl> ;; Transform data using Arl's higher-order functions
    arl> (define score-list (as.list scores))
    #> (85 92 78 90 88 76 95 89 91 87)

    arl> ;; Add 5 points to each score
    arl> (define adjusted-scores (map (lambda (x) (+ x 5)) score-list))
    #> (90 97 83 95 93 81 100 94 96 92)
    arl> (assert-equal (list 90 97 83 95 93 81 100 94 96 92) adjusted-scores)
    #> TRUE
    arl> (println (string-concat "After adding 5 points: " adjusted-scores))
    #> "After adding 5 points: (90 97 83 95 93 81 100 94 96 92)" 

    arl> ;; Filter passing scores (>= 80)
    arl> (define passing (filter (lambda (x) (>= x 80)) adjusted-scores))
    #> (90 97 83 95 93 81 100 94 96 92)
    arl> (assert-equal 10 (length passing))
    #> TRUE
    arl> (println (string-concat "Passing scores: " passing))
    #> "Passing scores: (90 97 83 95 93 81 100 94 96 92)" 

    arl> ;; Calculate average of passing scores
    arl> (define passing-avg (/ (reduce + passing) (length passing)))
    #> 92.1
    arl> (assert-equal 92.1 passing-avg)
    #> TRUE
    arl> (println (string-concat "Average of passing scores: " passing-avg))
    #> "Average of passing scores: 92.1" 

    arl> ;; ============================================================================
    arl> ;; Working with Named Lists (Data Frames)
    arl> ;; ============================================================================

    arl> (println "\n=== Working with Named Lists ===\n")
    #> "
    #> === Working with Named Lists ===
    #> " 

    arl> ;; Create a named list (similar to R's named vector)
    arl> (define student-data
    arl>   (dict
    arl>    :names (list "Alice" "Bob" "Carol" "Dave" "Eve")
    arl>    :scores (list 85 92 78 90 88)
    arl>    :ages (list 20 21 19 22 20)))
    #> ("Alice" "Bob" "Carol" "Dave" "Eve") (85 92 78 90 88) (20 21 19 22 20)

    arl> (println "Student data:")
    #> "Student data:" 
    arl> (println student-data)
    #> ("Alice" "Bob" "Carol" "Dave" "Eve") (85 92 78 90 88) (20 21 19 22 20) 

    arl> ;; Extract and process names
    arl> (define names (get "names" student-data))
    #> ("Alice" "Bob" "Carol" "Dave" "Eve")
    arl> (assert-equal (list "Alice" "Bob" "Carol" "Dave" "Eve") names)
    #> TRUE
    arl> (println (string-concat "\nStudent names: " names))
    #> "
    #> Student names: ("Alice" "Bob" "Carol" "Dave" "Eve")" 

    arl> ;; Extract and analyze scores
    arl> (define scores-data (get "scores" student-data))
    #> (85 92 78 90 88)
    arl> (assert-equal (list 85 92 78 90 88) scores-data)
    #> TRUE
    arl> (println (string-concat "Scores: " scores-data))
    #> "Scores: (85 92 78 90 88)" 

    arl> ;; ============================================================================
    arl> ;; Statistical Analysis
    arl> ;; ============================================================================

    arl> (println "\n=== Statistical Analysis ===\n")
    #> "
    #> === Statistical Analysis ===
    #> " 

    arl> ;; Create two groups
    arl> (define group-a (c 23 25 21 24 22 26 20))
    #> 23 25 21 24 22 26 20
    arl> (define group-b (c 30 32 28 31 29 33 27))
    #> 30 32 28 31 29 33 27

    arl> (assert-equal 23 (mean group-a))
    #> TRUE
    arl> (println (string-concat "Group A mean: " (mean group-a)))
    #> "Group A mean: 23" 
    arl> (assert-equal 30 (mean group-b))
    #> TRUE
    arl> (println (string-concat "Group B mean: " (mean group-b)))
    #> "Group B mean: 30" 

    arl> ;; Combine groups for comparison
    arl> (define all-values (c group-a group-b))
    #> 23 25 21 24 22 26 20 30 32 28 31 29 33 27
    arl> (println (string-concat "\nCombined range: " ((:: base range) all-values)))
    #> "
    #> Combined range: 20 33" 
    arl> (println (string-concat "Combined quantiles: " ((:: stats quantile) all-values)))
    #> "Combined quantiles: (:0% 20 :25% 23.25 :50% 26.5 :75% 29.75 :100% 33)" 

    arl> ;; ============================================================================
    arl> ;; Using R's seq and rep functions
    arl> ;; ============================================================================

    arl> (println "\n=== Sequences and Repetition ===\n")
    #> "
    #> === Sequences and Repetition ===
    #> " 

    arl> ;; Create sequences
    arl> (define seq-1-10 (seq 1 10))
    #> 1 2 3 4 5 6 7 8 9 10
    arl> (assert-equal (list 1 2 3 4 5 6 7 8 9 10) (as.list seq-1-10))
    #> TRUE
    arl> (println (string-concat "Sequence 1-10: " seq-1-10))
    #> "Sequence 1-10: 1 2 3 4 5 6 7 8 9 10" 

    arl> (define seq-by-2 (seq 0 20 2))
    #> 0 2 4 6 8 10 12 14 16 18 20
    arl> (assert-equal (list 0 2 4 6 8 10 12 14 16 18 20) (as.list seq-by-2))
    #> TRUE
    arl> (println (string-concat "Even numbers 0-20: " seq-by-2))
    #> "Even numbers 0-20: 0 2 4 6 8 10 12 14 16 18 20" 

    arl> ;; Repeat values
    arl> (define repeated (rep 5 10))
    #> 5 5 5 5 5 5 5 5 5 5
    arl> (assert-equal (list 5 5 5 5 5 5 5 5 5 5) (as.list repeated))
    #> TRUE
    arl> (println (string-concat "Repeat 5 ten times: " repeated))
    #> "Repeat 5 ten times: 5 5 5 5 5 5 5 5 5 5" 

    arl> ;; ============================================================================
    arl> ;; Combining Arl and R for Data Processing
    arl> ;; ============================================================================

    arl> (println "\n=== Hybrid Processing ===\n")
    #> "
    #> === Hybrid Processing ===
    #> " 

    arl> ;; Generate data in R
    arl> (define raw-data ((:: stats rnorm) 20 50 10))
    #> 42.895934363007 52.5688370915653 47.5330812153763 46.5245740060227 40.4838143273498 49.5497227519108 42.1509553054292 33.3205806341186 46.1977347971224 59.1899660906077 44.2465303739161 56.0796432222503 33.8211729171084 49.4443803447546 55.1940720394346 53.0115336216671 51.0567619414894 43.5929399169462 41.5029565396642 39.7587120939509

    arl> ;; Convert to list for Arl processing
    arl> (define data-list (as.list raw-data))
    #> (42.895934363007 52.5688370915653 47.5330812153763 46.5245740060227 40.4838143273498 49.5497227519108 42.1509553054292 33.3205806341186 46.1977347971224 59.1899660906077 44.2465303739161 56.0796432222503 33.8211729171084 49.4443803447546 55.1940720394346 53.0115336216671 51.0567619414894 43.5929399169462 41.5029565396642 39.7587120939509)

    arl> ;; Process with Arl functions
    arl> (define filtered (filter (lambda (x) (> x 50)) data-list))
    #> (52.5688370915653 59.1899660906077 56.0796432222503 55.1940720394346 53.0115336216671 51.0567619414894)
    arl> (define squared (map (lambda (x) (* x x)) filtered))
    #> (2763.48263315953 3503.45208580728 3144.92638393489 3046.3855882943 2810.22269692115 2606.79293994992)

    arl> (println (string-concat "Values > 50: " filtered))
    #> "Values > 50: (52.5688370915653 59.1899660906077 56.0796432222503 55.1940720394346 53.0115336216671 51.0567619414894)" 
    arl> (println (string-concat "Squared: " squared))
    #> "Squared: (2763.48263315953 3503.45208580728 3144.92638393489 3046.3855882943 2810.22269692115 2606.79293994992)" 

    arl> ;; Convert back to R vector for R functions
    arl> (define result-vec (unlist squared))
    #> 2763.48263315953 3503.45208580728 3144.92638393489 3046.3855882943 2810.22269692115 2606.79293994992
    arl> (println (string-concat "Mean of squared values: " (mean result-vec)))
    #> "Mean of squared values: 2979.21038801118" 

    arl> ;; ============================================================================
    arl> ;; Using keyword arguments (R named arguments)
    arl> ;; ============================================================================

    arl> (println "\n=== Keyword Arguments ===\n")
    #> "
    #> === Keyword Arguments ===
    #> " 

    arl> ;; R functions can be called with named arguments using keywords
    arl> (println "Creating sequences with named arguments:")
    #> "Creating sequences with named arguments:" 
    arl> (define seq-named (seq :from 1 :to 10 :by 2))
    #> 1 3 5 7 9
    arl> (assert-equal (list 1 3 5 7 9) (as.list seq-named))
    #> TRUE
    arl> (println (string-concat "seq(from=1, to=10, by=2): " seq-named))
    #> "seq(from=1, to=10, by=2): 1 3 5 7 9" 

    arl> ;; ============================================================================
    arl> ;; Data Aggregation Example
    arl> ;; ============================================================================

    arl> (println "\n=== Data Aggregation ===\n")
    #> "
    #> === Data Aggregation ===
    #> " 

    arl> ;; Sample transaction data
    arl> (define transactions
    arl>   (list
    arl>    (dict :id 1 :amount 100 :category "food")
    arl>    (dict :id 2 :amount 50 :category "transport")
    arl>    (dict :id 3 :amount 75 :category "food")
    arl>    (dict :id 4 :amount 200 :category "housing")
    arl>    (dict :id 5 :amount 30 :category "transport")))
    #> (1 100 "food" 2 50 "transport" 3 75 "food" 4 200 "housing" 5 30 "transport")

    arl> (println "Sample transactions:")
    #> "Sample transactions:" 
    arl> (println transactions)
    #> (1 100 "food" 2 50 "transport" 3 75 "food" 4 200 "housing" 5 30 "transport") 

    arl> ;; Extract all amounts
    arl> (define amounts (map (lambda (t) (get "amount" t)) transactions))
    #> (100 50 75 200 30)
    arl> (assert-equal (list 100 50 75 200 30) amounts)
    #> TRUE
    arl> (println (string-concat "\nAll amounts: " amounts))
    #> "
    #> All amounts: (100 50 75 200 30)" 

    arl> ;; Calculate total
    arl> (define total (reduce + amounts))
    #> 455
    arl> (assert-equal 455 total)
    #> TRUE
    arl> (println (string-concat "Total spent: $" total))
    #> "Total spent: $455" 

    arl> ;; Filter food transactions
    arl> (define food-trans (filter (lambda (t)
    arl>                              (= (get "category" t) "food"))
    arl>                            transactions))
    #> (1 100 "food" 3 75 "food")
    arl> (assert-equal 2 (length food-trans))
    #> TRUE
    arl> (println (string-concat "\nFood transactions: " (length food-trans) " items"))
    #> "
    #> Food transactions: 2 items" 

    arl> (define food-total (reduce + (map (lambda (t) (get "amount" t))
    arl>                                   food-trans)))
    #> 175
    arl> (assert-equal 175 food-total)
    #> TRUE
    arl> (println (string-concat "Food total: $" food-total))
    #> "Food total: $175" 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Graph Paths

BFS traversal and Dijkstra shortest paths.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/graph-paths.arl>

    arl> ;; Graph Paths Example
    arl> ;; Demonstrates BFS traversal and Dijkstra shortest paths

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import functional :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> (define contains?
    arl>   (lambda (lst value)
    arl>     (any? (lambda (x) (= x value)) lst)))
    #> <function>

    arl> (define remove-value
    arl>   (lambda (lst value)
    arl>     (filter (lambda (x) (not (= x value))) lst)))
    #> <function>

    arl> (define bfs
    arl>   (lambda (graph start)
    arl>     (define queue (list start))
    arl>     (define visited (list start))
    arl>     (define order (list start))
    arl>     (while (not (null? queue))
    arl>       (define node (car queue))
    arl>       (set! queue (cdr queue))
    arl>       (define edges (get node graph))
    arl>       (do-list (edge edges)
    arl>         (define neighbor (car edge))
    arl>         (if (not (contains? visited neighbor))
    arl>             (begin
    arl>               (set! visited (append visited (list neighbor)))
    arl>               (set! order (append order (list neighbor)))
    arl>               (set! queue (append queue (list neighbor)))))))
    arl>     order))
    #> <function>

    arl> (define min-distance
    arl>   (lambda (nodes dist-env)
    arl>     (reduce (lambda (best node)
    arl>               (if (< (get node dist-env)
    arl>                      (get best dist-env))
    arl>                   node
    arl>                   best))
    arl>             nodes)))
    #> <function>

    arl> (define dijkstra
    arl>   (lambda (graph nodes start)
    arl>     (define dist (new.env))
    arl>     (define prev (new.env))
    arl>     (do-list (n nodes)
    arl>       (assign n 9999 dist)
    arl>       (assign n #nil prev))
    arl>     (assign start 0 dist)
    arl>     (define unvisited nodes)
    arl>     (while (not (null? unvisited))
    arl>       (define current (min-distance unvisited dist))
    arl>       (set! unvisited (remove-value unvisited current))
    arl>       (define edges (get current graph))
    arl>       (do-list (edge edges)
    arl>         (define neighbor (car edge))
    arl>         (define weight (car (cdr edge)))
    arl>         (define alt (+ (get current dist) weight))
    arl>         (if (< alt (get neighbor dist))
    arl>             (begin
    arl>               (assign neighbor alt dist)
    arl>               (assign neighbor current prev)))))
    arl>     (dict :dist dist :prev prev)))
    #> <function>

    arl> (define reconstruct-path
    arl>   (lambda (prev-env start goal)
    arl>     (define current goal)
    arl>     (define path (list current))
    arl>     (while (not (= current start))
    arl>       (define parent (get current prev-env))
    arl>       (if (null? parent)
    arl>           (begin
    arl>             (set! path (list))
    arl>             (set! current start))
    arl>           (begin
    arl>             (set! path (cons parent path))
    arl>             (set! current parent))))
    arl>     path))
    #> <function>

    arl> (define graph
    arl>   (dict
    arl>    :A (list (list "B" 1) (list "C" 2))
    arl>    :B (list (list "C" 1) (list "D" 4) (list "E" 6))
    arl>    :C (list (list "D" 5) (list "E" 7))
    arl>    :D (list (list "E" 2))
    arl>    :E (list)))
    #> (("B" 1) ("C" 2)) (("C" 1) ("D" 4) ("E" 6)) (("D" 5) ("E" 7)) (("E" 2)) ()

    arl> (define nodes (list "A" "B" "C" "D" "E"))
    #> ("A" "B" "C" "D" "E")

    arl> (println "=== Graph Traversal ===")
    #> "=== Graph Traversal ===" 
    arl> (define bfs-order (bfs graph "A"))
    #> ("A" "B" "C" "D" "E")
    arl> (assert-equal (list "A" "B" "C" "D" "E") bfs-order)
    #> TRUE
    arl> (println (string-concat "BFS order from A: " bfs-order))
    #> "BFS order from A: ("A" "B" "C" "D" "E")" 

    arl> (println "\n=== Shortest Paths ===")
    #> "
    #> === Shortest Paths ===" 
    arl> (define dijkstra-result (dijkstra graph nodes "A"))
    #> <environment> <environment>
    arl> (define dist-env (get "dist" dijkstra-result))
    #> <environment>
    arl> (define prev-env (get "prev" dijkstra-result))
    #> <environment>
    arl> (assert-equal 0 (get "A" dist-env))
    #> TRUE
    arl> (assert-equal 1 (get "B" dist-env))
    #> TRUE
    arl> (assert-equal 2 (get "C" dist-env))
    #> TRUE
    arl> (assert-equal 5 (get "D" dist-env))
    #> TRUE
    arl> (assert-equal 7 (get "E" dist-env))
    #> TRUE
    arl> (define shortest-path (reconstruct-path prev-env "A" "E"))
    #> ("A" "B" "E")
    arl> (define shortest-cost (get "E" dist-env))
    #> 7
    arl> (assert-equal (list "A" "B" "E") shortest-path)
    #> TRUE
    arl> (assert-equal 7 shortest-cost)
    #> TRUE

    arl> (println (string-concat "Shortest path A -> E: " shortest-path))
    #> "Shortest path A -> E: ("A" "B" "E")" 
    arl> (println (string-concat "Total cost: " shortest-cost))
    #> "Total cost: 7" 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Log Parser

Parsing logs, aggregating status counts, and computing averages.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/log-parser.arl>

    arl> ;; Log Parser Example
    arl> ;; Demonstrates text processing and simple analytics

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import strings :refer :all)
    arl> (import threading :refer :all)

    arl> (define log-lines
    arl>   (list
    arl>    "2026-01-01 GET /api/users 200 100"
    arl>    "2026-01-01 POST /api/login 500 110"
    arl>    "2026-01-01 GET /api/users 200 120"
    arl>    "2026-01-01 PUT /api/users 200 130"
    arl>    "2026-01-01 GET /api/orders 500 140"
    arl>    "2026-01-01 GET /api/orders 200 120"))
    #> ("2026-01-01 GET /api/users 200 100" "2026-01-01 POST /api/login 500 110" "2026-01-01 GET /api/users 200 120" "2026-01-01 PUT /api/users 200 130" "2026-01-01 GET /api/orders 500 140" "2026-01-01 GET /api/orders 200 120")

    arl> (define parse-line
    arl>   (lambda (line)
    arl>     (define parts (string-split line " "))
    arl>     (dict
    arl>      :date (nth parts 0)
    arl>      :method (nth parts 1)
    arl>      :path (nth parts 2)
    arl>      :status (as.numeric (nth parts 3))
    arl>      :latency (as.numeric (nth parts 4)))))
    #> <function>

    arl> (define parsed (map parse-line log-lines))
    #> ("2026-01-01" "GET" "/api/users" 200 100 "2026-01-01" "POST" "/api/login" 500 110 "2026-01-01" "GET" "/api/users" 200 120 "2026-01-01" "PUT" "/api/users" 200 130 "2026-01-01" "GET" "/api/orders" 500 140 "2026-01-01" "GET" "/api/orders" 200 120)
    arl> (assert-equal 6 (length parsed))
    #> TRUE

    arl> (define statuses (map (lambda (row) (get "status" row)) parsed))
    #> (200 500 200 200 500 200)
    arl> (define latencies (map (lambda (row) (get "latency" row)) parsed))
    #> (100 110 120 130 140 120)
    arl> (assert-equal (list 200 500 200 200 500 200) statuses)
    #> TRUE
    arl> (assert-equal (list 100 110 120 130 140 120) latencies)
    #> TRUE

    arl> (define status-500 (length (filter (lambda (s) (= s 500)) statuses)))
    #> 2
    arl> (define avg-latency (/ (reduce + latencies) (length latencies)))
    #> 120
    arl> (assert-equal 2 status-500)
    #> TRUE
    arl> (assert-equal 120 avg-latency)
    #> TRUE

    arl> (println "=== Log Summary ===")
    #> "=== Log Summary ===" 
    arl> (assert-equal 6 (length log-lines))
    #> TRUE
    arl> (println (string-concat "Total lines: " (length log-lines)))
    #> "Total lines: 6" 
    arl> (println (string-concat "HTTP 500 count: " status-500))
    #> "HTTP 500 count: 2" 
    arl> (println (string-concat "Average latency: " avg-latency))
    #> "Average latency: 120" 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Sales Report

Data wrangling and CSV output using R interop.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/sales-report.arl>

    arl> ;; Sales Report Example
    arl> ;; Demonstrates data wrangling with R interop and file output

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> (define transactions
    arl>   (list
    arl>    (dict :product "beta" :region "east" :amount 120)
    arl>    (dict :product "alpha" :region "west" :amount 80)
    arl>    (dict :product "beta" :region "north" :amount 150)
    arl>    (dict :product "gamma" :region "east" :amount 60)
    arl>    (dict :product "beta" :region "west" :amount 90)
    arl>    (dict :product "alpha" :region "north" :amount 110)
    arl>    (dict :product "gamma" :region "south" :amount 50)
    arl>    (dict :product "beta" :region "south" :amount 30)))
    #> ("beta" "east" 120 "alpha" "west" 80 "beta" "north" 150 "gamma" "east" 60 "beta" "west" 90 "alpha" "north" 110 "gamma" "south" 50 "beta" "south" 30)

    arl> (println "=== Sales Transactions ===")
    #> "=== Sales Transactions ===" 
    arl> (println transactions)
    #> ("beta" "east" 120 "alpha" "west" 80 "beta" "north" 150 "gamma" "east" 60 "beta" "west" 90 "alpha" "north" 110 "gamma" "south" 50 "beta" "south" 30) 

    arl> (define products (map (lambda (t) (get "product" t)) transactions))
    #> ("beta" "alpha" "beta" "gamma" "beta" "alpha" "gamma" "beta")
    arl> (define regions (map (lambda (t) (get "region" t)) transactions))
    #> ("east" "west" "north" "east" "west" "north" "south" "south")
    arl> (define amounts (map (lambda (t) (get "amount" t)) transactions))
    #> (120 80 150 60 90 110 50 30)
    arl> (define products-vec (unlist products))
    #> "beta" "alpha" "beta" "gamma" "beta" "alpha" "gamma" "beta"
    arl> (define regions-vec (unlist regions))
    #> "east" "west" "north" "east" "west" "north" "south" "south"
    arl> (define amounts-vec (unlist amounts))
    #> 120 80 150 60 90 110 50 30

    arl> (define total-sales (reduce + amounts))
    #> 690
    arl> (assert-equal 690 total-sales)
    #> TRUE

    arl> (define totals-env
    arl>   (new.env :parent (emptyenv)))
    #> <environment>
    arl> (do-list (t transactions)
    arl>   (define product (get "product" t))
    arl>   (define amount (get "amount" t))
    arl>   (define has (exists product :envir totals-env :inherits #f))
    arl>   (if has
    arl>       (assign product (+ (get product totals-env #f) amount)
    arl>               totals-env)
    arl>       (assign product amount totals-env)))

    arl> (assert-equal 190 (get "alpha" totals-env #f))
    #> TRUE
    arl> (assert-equal 390 (get "beta" totals-env #f))
    #> TRUE
    arl> (assert-equal 110 (get "gamma" totals-env #f))
    #> TRUE

    arl> (define product-names (ls totals-env))
    #> "alpha" "beta" "gamma"
    arl> (define product-names-list (as.list product-names))
    #> ("alpha" "beta" "gamma")
    arl> (define totals-list
    arl>   (map (lambda (name)
    arl>          (get name totals-env #f))
    arl>        product-names-list))
    #> (190 390 110)
    arl> (define totals-vec (unlist totals-list))
    #> 190 390 110
    arl> (define top-index (which.max totals-vec))
    #> 2
    arl> ;; r-call is handy for calling R operators with special names like "["
    arl> (define top-product (r-call "[" (list product-names top-index)))
    #> "beta"
    arl> (assert-equal "beta" top-product)
    #> TRUE

    arl> (define totals-by-product (list))
    #> ()
    arl> (do-list (name product-names-list)
    arl>   (set! totals-by-product
    arl>         (append totals-by-product
    arl>                 (list (dict :product name
    arl>                             :total (get name totals-env #f))))))

    arl> (println "\n=== Summary ===")
    #> "
    #> === Summary ===" 
    arl> (println (string-concat "Total sales: " total-sales))
    #> "Total sales: 690" 
    arl> (println (string-concat "Totals by product: " totals-by-product))
    #> "Totals by product: ("alpha" 190 "beta" 390 "gamma" 110)" 
    arl> (println (string-concat "Top product: " top-product))
    #> "Top product: beta" 

    arl> (define sales-df
    arl>   (data.frame :product products-vec :region regions-vec :amount amounts-vec))
    #> (:product "beta" "alpha" "beta" "gamma" "beta" "alpha" "gamma" "beta" :region "east" "west" "north" "east" "west" "north" "south" "south" :amount 120 80 150 60 90 110 50 30)

    arl> (define report-path (file.path (tempdir) "sales-report.csv"))
    #> "/tmp/RtmpVyfziX/sales-report.csv"
    arl> ((:: utils write.csv) sales-df report-path #f)
    arl> (assert-true (file.exists report-path))
    #> TRUE

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 

## Task Runner

Dependency ordering and execution sequencing.

Source:
<https://github.com/wwbrannon/arl/blob/main/inst/examples/task-runner.arl>

    arl> ;; Task Runner Example
    arl> ;; Demonstrates simple dependency resolution and execution ordering

    arl> (import assert :refer :all)
    arl> (import binding :refer :all)
    arl> (import control :refer :all)
    arl> (import dict :refer :all)
    arl> (import display :refer :all)
    arl> (import looping :refer :all)
    arl> (import threading :refer :all)

    arl> (define contains?
    arl>   (lambda (lst value)
    arl>     (any? (lambda (x) (= x value)) lst)))
    #> <function>

    arl> (define tasks
    arl>   (list
    arl>    (dict :name "clean" :deps (list))
    arl>    (dict :name "compile" :deps (list "clean"))
    arl>    (dict :name "test" :deps (list "compile"))
    arl>    (dict :name "package" :deps (list "test"))
    arl>    (dict :name "deploy" :deps (list "package"))))
    #> ("clean" () "compile" ("clean") "test" ("compile") "package" ("test") "deploy" ("package"))

    arl> (define task-by-name
    arl>   (lambda (name)
    arl>     (car (filter (lambda (t) (= (get "name" t) name)) tasks))))
    #> <function>

    arl> (define visited (list))
    #> ()
    arl> (define run-order (list))
    #> ()

    arl> (define visit
    arl>   (lambda (name)
    arl>     (if (contains? visited name)
    arl>         #nil
    arl>         (begin
    arl>           (set! visited (append visited (list name)))
    arl>           (define task (task-by-name name))
    arl>           (define deps (get "deps" task))
    arl>           (do-list (dep deps)
    arl>             (visit dep))
    arl>           (set! run-order (append run-order (list name)))))))
    #> <function>

    arl> (println "=== Task Runner ===")
    #> "=== Task Runner ===" 
    arl> (do-list (t tasks)
    arl>   (visit (get "name" t)))
    arl> (assert-equal (list "clean" "compile" "test" "package" "deploy") run-order)
    #> TRUE
    arl> (assert-equal 5 (length run-order))
    #> TRUE
    arl> (println (string-concat "Execution order: " run-order))
    #> "Execution order: ("clean" "compile" "test" "package" "deploy")" 

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 
