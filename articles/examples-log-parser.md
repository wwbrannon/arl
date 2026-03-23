# Example: Log Parser

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
