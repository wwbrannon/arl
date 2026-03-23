# Example: Graph Paths

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
