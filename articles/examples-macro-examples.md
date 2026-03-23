# Example: Macro Examples

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
    #> ((lambda (tmp__17) (define x tmp__17) (begin (+ x 2))) 1) 

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
    #> "Generated symbol 1: G__23" 
    arl> (println (string-concat "Generated symbol 2: " (gensym)))
    #> "Generated symbol 2: G__24" 
    arl> (println (string-concat "Generated symbol with prefix: " (gensym "my-prefix")))
    #> "Generated symbol with prefix: my-prefix__25" 

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
