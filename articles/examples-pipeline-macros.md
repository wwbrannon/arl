# Example: Pipeline Macros

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
