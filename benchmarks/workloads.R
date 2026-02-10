# Pre-defined Workloads for Benchmarking
# Standard workloads for testing Rye performance

# Source benchmark helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  # Try installed package
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "rye")
  if (helpers_path != "") {
    source(helpers_path)
  }
}

# Micro workload - baseline overhead
WORKLOAD_MICRO <- "(+ 1 2)"

# Small workload - recursive fibonacci
WORKLOAD_SMALL <- '
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2))))))

(fib 10)
'

# Medium workload - quicksort with data
WORKLOAD_MEDIUM <- '
(define partition (lambda (lst pivot)
  (if (null? lst)
    (list (list) (list))
    (if (< (car lst) pivot)
      (let ((rest (partition (cdr lst) pivot)))
        (list (cons (car lst) (car rest))
              (car (cdr rest))))
      (let ((rest (partition (cdr lst) pivot)))
        (list (car rest)
              (cons (car lst) (car (cdr rest)))))))))

(define quicksort (lambda (lst)
  (if (null? lst)
    (list)
    (let* ((pivot (car lst))
           (parts (partition (cdr lst) pivot)))
      (append (quicksort (car parts))
              (list pivot)
              (quicksort (car (cdr parts))))))))

(quicksort (list 5 2 8 1 9 3 7 4 6))
'

# Large workload - macro expansion examples
WORKLOAD_LARGE <- '
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))

(defmacro unless (test . body)
  `(if ,test #nil (begin ,@body)))

(defmacro cond clauses
  (if (null? clauses)
    #nil
    (let ((clause (car clauses)))
      (if (eq? (car clause) else)
        `(begin ,@(cdr clause))
        `(if ,(car clause)
           (begin ,@(cdr clause))
           (cond ,@(cdr clauses)))))))

(defmacro let* (bindings . body)
  (if (null? bindings)
    `(begin ,@body)
    `(let (,(car bindings))
       (let* ,(cdr bindings) ,@body))))

(define test-macros (lambda ()
  (when #t
    (unless #f
      (cond
        (#f 1)
        (#f 2)
        (else 3))))
  (let* ((x 1) (y 2) (z 3))
    (+ x y z))))

(test-macros)
'

# XL workload - synthetic nested lists
WORKLOAD_XL <- function() {
  # Generate 2000-element nested structure
  elements <- character(2000)
  for (i in seq_len(2000)) {
    elements[i] <- as.character(i)
  }
  paste0("(list ", paste(elements, collapse = " "), ")")
}

# Real workloads from example files
get_real_workload <- function(name) {
  examples <- c(
    "fibonacci" = "fibonacci.rye",
    "quicksort" = "quicksort.rye",
    "graph-paths" = "graph-paths.rye",
    "macro-examples" = "macro-examples.rye",
    "data-analysis" = "data-analysis.rye",
    "oop-simple" = "oop-simple.rye"
  )

  if (!(name %in% names(examples))) {
    stop("Unknown workload name: ", name, ". Available: ",
         paste(names(examples), collapse = ", "))
  }

  load_example_workload(examples[[name]]) # nolint: object_usage_linter.
}

# String-heavy workload
WORKLOAD_STRING_HEAVY <- function(size = 1000) {
  str_content <- paste(rep("x", size), collapse = "")
  paste0('(str "', str_content, '" "', str_content, '")')
}

# Deep recursion workload
WORKLOAD_DEEP_RECURSION <- '
(define deep (lambda (n acc)
  (if (= n 0)
    acc
    (deep (- n 1) (+ acc 1)))))

(deep 500 0)
'

# Many arguments workload
WORKLOAD_MANY_ARGS <- function(n_args = 50) {
  args <- paste(seq_len(n_args), collapse = " ")
  paste0("(+ ", args, ")")
}

# Helper to get all workloads as a list
get_all_workloads <- function() {
  list(
    micro = WORKLOAD_MICRO,
    small = WORKLOAD_SMALL,
    medium = WORKLOAD_MEDIUM,
    large = WORKLOAD_LARGE,
    xl = WORKLOAD_XL(),
    string_1k = WORKLOAD_STRING_HEAVY(1000),
    string_10k = WORKLOAD_STRING_HEAVY(10000),
    deep_recursion = WORKLOAD_DEEP_RECURSION,
    many_args_50 = WORKLOAD_MANY_ARGS(50),
    many_args_100 = WORKLOAD_MANY_ARGS(100)
  )
}

# Helper to get real example workloads
get_real_workloads <- function() {
  tryCatch({
    list(
      fibonacci = get_real_workload("fibonacci"),
      quicksort = get_real_workload("quicksort"),
      graph_paths = get_real_workload("graph-paths"),
      macro_examples = get_real_workload("macro-examples")
    )
  }, error = function(e) {
    message("Could not load real workloads (package may not be installed): ", e$message)
    list()
  })
}
