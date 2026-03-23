# Example: Task Runner

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
