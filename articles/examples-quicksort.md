# Example: Quicksort

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
