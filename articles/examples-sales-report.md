# Example: Sales Report

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
    #> "/tmp/RtmptpJUBd/sales-report.csv"
    arl> ((:: utils write.csv) sales-df report-path #f)
    arl> (assert-true (file.exists report-path))
    #> TRUE

    arl> (println "\nExample complete!")
    #> "
    #> Example complete!" 
