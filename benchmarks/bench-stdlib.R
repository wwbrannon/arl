# Standard Library Benchmarks
# Benchmarks for stdlib functions (list ops, higher-order functions, etc.)

library(arl)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Standard Library Benchmarks ===\n\n")

# Benchmark 1: List operations on varying sizes
cat("Benchmark 1: List operations\n")
engine1 <- Engine$new()

# Create test lists
engine1$eval_text('(define list10 (list 1 2 3 4 5 6 7 8 9 10))')
engine1$eval_text('(define list100 (range 1 100))')
engine1$eval_text('(define list1000 (range 1 1000))')

bench_list_ops <- benchmark_component(
  "car (10)" = engine1$eval(engine1$read("(car list10)")[[1]]),
  "cdr (10)" = engine1$eval(engine1$read("(cdr list10)")[[1]]),
  "length (10)" = engine1$eval(engine1$read("(length list10)")[[1]]),
  "length (100)" = engine1$eval(engine1$read("(length list100)")[[1]]),
  "length (1000)" = engine1$eval(engine1$read("(length list1000)")[[1]]),
  "reverse (10)" = engine1$eval(engine1$read("(reverse list10)")[[1]]),
  "reverse (100)" = engine1$eval(engine1$read("(reverse list100)")[[1]]),
  iterations = 500,
  check = FALSE
)
print(bench_list_ops[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Higher-order functions (map, filter, reduce)
cat("Benchmark 2: Higher-order functions\n")
engine2 <- Engine$new()

engine2$eval_text('(define list10 (list 1 2 3 4 5 6 7 8 9 10))')
engine2$eval_text('(define list100 (range 1 100))')
engine2$eval_text('(define list1000 (range 1 1000))')
engine2$eval_text('(define inc (lambda (x) (+ x 1)))')
engine2$eval_text('(define even? (lambda (x) (= (%% x 2) 0)))')
engine2$eval_text('(define add (lambda (a b) (+ a b)))')

bench_higher_order <- benchmark_component(
  "map (10)" = engine2$eval(engine2$read("(map inc list10)")[[1]]),
  "map (100)" = engine2$eval(engine2$read("(map inc list100)")[[1]]),
  "map (1000)" = engine2$eval(engine2$read("(map inc list1000)")[[1]]),
  "filter (10)" = engine2$eval(engine2$read("(filter even? list10)")[[1]]),
  "filter (100)" = engine2$eval(engine2$read("(filter even? list100)")[[1]]),
  "reduce (10)" = engine2$eval(engine2$read("(reduce add list10 0)")[[1]]),
  "reduce (100)" = engine2$eval(engine2$read("(reduce add list100 0)")[[1]]),
  iterations = 100,
  check = FALSE
)
print(bench_higher_order[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Function composition
cat("Benchmark 3: Function composition\n")
engine3 <- Engine$new()

engine3$eval_text('
(define inc (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))
')

bench_compose <- benchmark_component(
  "Single function" = engine3$eval(engine3$read("(inc 5)")[[1]]),
  "Two composed" = engine3$eval(engine3$read("(double (inc 5))")[[1]]),
  "Three composed" = engine3$eval(engine3$read("(square (double (inc 5)))")[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_compose[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: String operations
cat("Benchmark 4: String operations\n")
engine4 <- Engine$new()

bench_strings <- benchmark_component(
  "str (2 args)" = engine4$eval(engine4$read('(str "hello" "world")')[[1]]),
  "str (5 args)" = engine4$eval(engine4$read('(str "a" "b" "c" "d" "e")')[[1]]),
  "str (10 args)" = engine4$eval(engine4$read('(str "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")')[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_strings[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Predicates
cat("Benchmark 5: Predicates\n")
engine5 <- Engine$new()

bench_predicates <- benchmark_component(
  "null?" = engine5$eval(engine5$read("(null? #nil)")[[1]]),
  "list?" = engine5$eval(engine5$read("(list? (list 1 2 3))")[[1]]),
  "number?" = engine5$eval(engine5$read("(number? 42)")[[1]]),
  "string?" = engine5$eval(engine5$read('(string? "hello")')[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_predicates[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 6: List construction
cat("Benchmark 6: List construction\n")
engine6 <- Engine$new()

bench_construction <- benchmark_component(
  "cons (single)" = engine6$eval(engine6$read("(cons 1 (list 2 3))")[[1]]),
  "append (2 lists)" = engine6$eval(engine6$read("(append (list 1 2) (list 3 4))")[[1]]),
  "append (large)" = engine6$eval(engine6$read("(append (range 1 50) (range 51 100))")[[1]]),
  "range (10)" = engine6$eval(engine6$read("(range 1 10)")[[1]]),
  "range (100)" = engine6$eval(engine6$read("(range 1 100)")[[1]]),
  iterations = 500,
  check = FALSE
)
print(bench_construction[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 7: Nested operations
cat("Benchmark 7: Nested higher-order operations\n")
engine7 <- Engine$new()

engine7$eval_text('(define list10 (range 1 10))')
engine7$eval_text('(define inc (lambda (x) (+ x 1)))')
engine7$eval_text('(define even? (lambda (x) (= (%% x 2) 0)))')
engine7$eval_text('(define add (lambda (a b) (+ a b)))')

bench_nested <- benchmark_component(
  "map then filter" = engine7$eval(engine7$read("(filter even? (map inc list10))")[[1]]),
  "filter then map" = engine7$eval(engine7$read("(map inc (filter even? list10))")[[1]]),
  "map, filter, reduce" = engine7$eval(engine7$read("(reduce add (filter even? (map inc list10)) 0)")[[1]]),
  iterations = 500,
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
stdlib_results <- list(
  list_ops = bench_list_ops,
  higher_order = bench_higher_order,
  compose = bench_compose,
  strings = bench_strings,
  predicates = bench_predicates,
  construction = bench_construction,
  nested = bench_nested
)

# Save results
save_benchmark_results(stdlib_results, "stdlib")

cat("=== Standard Library Benchmarks Complete ===\n")
