# Example: FizzBuzz

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
