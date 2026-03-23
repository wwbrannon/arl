# Example: Data Analysis

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
