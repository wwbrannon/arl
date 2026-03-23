# Example: Fibonacci

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
