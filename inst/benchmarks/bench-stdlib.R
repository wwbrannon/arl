# Standard Library Benchmarks
# Benchmarks for stdlib functions (list ops, higher-order functions, etc.)

library(rye)
source("inst/benchmarks/benchmark-helpers.R")
source("inst/benchmarks/workloads.R")

cat("=== Standard Library Benchmarks ===\n\n")

# Set up environment
setup_env <- function() {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)
  env
}

# Benchmark 1: List operations on varying sizes
cat("Benchmark 1: List operations\n")
env1 <- setup_env()

# Create test lists
rye:::rye_eval_text('(define list10 (list 1 2 3 4 5 6 7 8 9 10))', env1)
rye:::rye_eval_text('(define list100 (range 1 100))', env1)
rye:::rye_eval_text('(define list1000 (range 1 1000))', env1)

bench_list_ops <- benchmark_component(
  "car (10)" = rye_eval(rye_read("(car list10)")[[1]], env1),
  "cdr (10)" = rye_eval(rye_read("(cdr list10)")[[1]], env1),
  "length (10)" = rye_eval(rye_read("(length list10)")[[1]], env1),
  "length (100)" = rye_eval(rye_read("(length list100)")[[1]], env1),
  "length (1000)" = rye_eval(rye_read("(length list1000)")[[1]], env1),
  "reverse (10)" = rye_eval(rye_read("(reverse list10)")[[1]], env1),
  "reverse (100)" = rye_eval(rye_read("(reverse list100)")[[1]], env1),
  iterations = 500,
  check = FALSE
)
print(bench_list_ops[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Higher-order functions (map, filter, reduce)
cat("Benchmark 2: Higher-order functions\n")
env2 <- setup_env()

rye:::rye_eval_text('(define list10 (list 1 2 3 4 5 6 7 8 9 10))', env2)
rye:::rye_eval_text('(define list100 (range 1 100))', env2)
rye:::rye_eval_text('(define list1000 (range 1 1000))', env2)
rye:::rye_eval_text('(define inc (lambda (x) (+ x 1)))', env2)
rye:::rye_eval_text('(define even? (lambda (x) (= (%% x 2) 0)))', env2)
rye:::rye_eval_text('(define add (lambda (a b) (+ a b)))', env2)

bench_higher_order <- benchmark_component(
  "map (10)" = rye_eval(rye_read("(map inc list10)")[[1]], env2),
  "map (100)" = rye_eval(rye_read("(map inc list100)")[[1]], env2),
  "map (1000)" = rye_eval(rye_read("(map inc list1000)")[[1]], env2),
  "filter (10)" = rye_eval(rye_read("(filter even? list10)")[[1]], env2),
  "filter (100)" = rye_eval(rye_read("(filter even? list100)")[[1]], env2),
  "reduce (10)" = rye_eval(rye_read("(reduce add 0 list10)")[[1]], env2),
  "reduce (100)" = rye_eval(rye_read("(reduce add 0 list100)")[[1]], env2),
  iterations = 100,
  check = FALSE
)
print(bench_higher_order[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Function composition
cat("Benchmark 3: Function composition\n")
env3 <- setup_env()

rye:::rye_eval_text('
(define inc (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))
', env3)

bench_compose <- benchmark_component(
  "Single function" = rye_eval(rye_read("(inc 5)")[[1]], env3),
  "Two composed" = rye_eval(rye_read("(double (inc 5))")[[1]], env3),
  "Three composed" = rye_eval(rye_read("(square (double (inc 5)))")[[1]], env3),
  iterations = 1000,
  check = FALSE
)
print(bench_compose[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: String operations
cat("Benchmark 4: String operations\n")
env4 <- setup_env()

bench_strings <- benchmark_component(
  "str (2 args)" = rye_eval(rye_read('(str "hello" "world")')[[1]], env4),
  "str (5 args)" = rye_eval(rye_read('(str "a" "b" "c" "d" "e")')[[1]], env4),
  "str (10 args)" = rye_eval(rye_read('(str "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")')[[1]], env4),
  iterations = 1000,
  check = FALSE
)
print(bench_strings[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Predicates
cat("Benchmark 5: Predicates\n")
env5 <- setup_env()

bench_predicates <- benchmark_component(
  "null?" = rye_eval(rye_read("(null? #nil)")[[1]], env5),
  "list?" = rye_eval(rye_read("(list? (list 1 2 3))")[[1]], env5),
  "number?" = rye_eval(rye_read("(number? 42)")[[1]], env5),
  "string?" = rye_eval(rye_read('(string? "hello")')[[1]], env5),
  iterations = 1000,
  check = FALSE
)
print(bench_predicates[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 6: List construction
cat("Benchmark 6: List construction\n")
env6 <- setup_env()

bench_construction <- benchmark_component(
  "cons (single)" = rye_eval(rye_read("(cons 1 (list 2 3))")[[1]], env6),
  "append (2 lists)" = rye_eval(rye_read("(append (list 1 2) (list 3 4))")[[1]], env6),
  "append (large)" = rye_eval(rye_read("(append (range 1 50) (range 51 100))")[[1]], env6),
  "range (10)" = rye_eval(rye_read("(range 1 10)")[[1]], env6),
  "range (100)" = rye_eval(rye_read("(range 1 100)")[[1]], env6),
  iterations = 500,
  check = FALSE
)
print(bench_construction[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 7: Nested operations
cat("Benchmark 7: Nested higher-order operations\n")
env7 <- setup_env()

rye:::rye_eval_text('(define list10 (range 1 10))', env7)
rye:::rye_eval_text('(define inc (lambda (x) (+ x 1)))', env7)
rye:::rye_eval_text('(define even? (lambda (x) (= (mod x 2) 0)))', env7)
rye:::rye_eval_text('(define add (lambda (a b) (+ a b)))', env7)

bench_nested <- benchmark_component(
  "map then filter" = rye_eval(rye_read("(filter even? (map inc list10))")[[1]], env7),
  "filter then map" = rye_eval(rye_read("(map inc (filter even? list10))")[[1]], env7),
  "map, filter, reduce" = rye_eval(rye_read("(reduce add 0 (filter even? (map inc list10)))")[[1]], env7),
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
