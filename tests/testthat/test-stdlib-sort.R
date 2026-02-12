# Sorting tests: sort, sort-by, stable-sort, merge

engine <- make_engine()

test_that("sort works with comparator function", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  result <- engine$eval_in_env(
    engine$read("(sort '(3 1 4 1 5) <)")[[1]], env)
  expect_equal(result, list(1, 1, 3, 4, 5))

  # Descending sort
  result <- engine$eval_in_env(
    engine$read("(sort '(3 1 4 1 5) >)")[[1]], env)
  expect_equal(result, list(5, 4, 3, 1, 1))

  # Empty list
  result <- engine$eval_in_env(
    engine$read("(sort '() <)")[[1]], env)
  expect_equal(result, list())

  # Single element
  result <- engine$eval_in_env(
    engine$read("(sort '(42) <)")[[1]], env)
  expect_equal(result, list(42))
})

test_that("sort-by works with key function and comparator", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  # Sort by absolute value
  result <- engine$eval_in_env(
    engine$read("(sort-by '(-3 1 -4 2) abs <)")[[1]], env)
  expect_equal(result, list(1, 2, -3, -4))

  # Sort strings by length (all distinct lengths to avoid unstable ordering)
  result <- engine$eval_in_env(
    engine$read("(sort-by '(\"hello\" \"hi\" \"a\" \"goodbye\") nchar <)")[[1]], env)
  expect_equal(result, list("a", "hi", "hello", "goodbye"))
})

test_that("stable-sort preserves order of equal elements", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  # Basic sorting
  result <- engine$eval_in_env(
    engine$read("(stable-sort '(3 1 4 1 5) <)")[[1]], env)
  expect_equal(result, list(1, 1, 3, 4, 5))

  # Stability test: sort pairs by first element, verify original order preserved for equal keys
  result <- engine$eval_in_env(
    engine$read("(stable-sort (list (list 1 'a) (list 2 'b) (list 1 'c) (list 2 'd))
                   (lambda (x y) (< (car x) (car y))))")[[1]], env)
  # Elements with key 1 should appear in original order: (1 a) before (1 c)
  # Elements with key 2 should appear in original order: (2 b) before (2 d)
  expect_equal(result[[1]], list(1, quote(a)))
  expect_equal(result[[2]], list(1, quote(c)))
  expect_equal(result[[3]], list(2, quote(b)))
  expect_equal(result[[4]], list(2, quote(d)))
})

test_that("stable-sort handles edge cases", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  # Empty list
  result <- engine$eval_in_env(engine$read("(stable-sort '() <)")[[1]], env)
  expect_equal(result, list())

  # Single element
  result <- engine$eval_in_env(engine$read("(stable-sort '(42) <)")[[1]], env)
  expect_equal(result, list(42))

  # Two elements already sorted
  result <- engine$eval_in_env(engine$read("(stable-sort '(1 2) <)")[[1]], env)
  expect_equal(result, list(1, 2))

  # Two elements reversed
  result <- engine$eval_in_env(engine$read("(stable-sort '(2 1) <)")[[1]], env)
  expect_equal(result, list(1, 2))

  # Already sorted
  result <- engine$eval_in_env(engine$read("(stable-sort '(1 2 3 4 5) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3, 4, 5))

  # Reverse sorted
  result <- engine$eval_in_env(engine$read("(stable-sort '(5 4 3 2 1) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3, 4, 5))

  # All equal elements
  result <- engine$eval_in_env(engine$read("(stable-sort '(7 7 7 7) <)")[[1]], env)
  expect_equal(result, list(7, 7, 7, 7))

  # Descending sort
  result <- engine$eval_in_env(engine$read("(stable-sort '(3 1 4 1 5) >)")[[1]], env)
  expect_equal(result, list(5, 4, 3, 1, 1))

  # Negative numbers
  result <- engine$eval_in_env(engine$read("(stable-sort '(3 -1 0 -5 2) <)")[[1]], env)
  expect_equal(result, list(-5, -1, 0, 2, 3))

  # Strings (lexicographic)
  result <- engine$eval_in_env(
    engine$read('(stable-sort (list "banana" "apple" "cherry") string<?)')[[1]], env)
  expect_equal(result, list("apple", "banana", "cherry"))
})

test_that("stable-sort stability across various patterns", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  # Many equal keys: all 1s with different second elements
  result <- engine$eval_in_env(
    engine$read("(stable-sort (list (list 1 'a) (list 1 'b) (list 1 'c) (list 1 'd))
                   (lambda (x y) (< (car x) (car y))))")[[1]], env)
  # All equal key - original order must be preserved
  expect_equal(as.character(result[[1]][[2]]), "a")
  expect_equal(as.character(result[[2]][[2]]), "b")
  expect_equal(as.character(result[[3]][[2]]), "c")
  expect_equal(as.character(result[[4]][[2]]), "d")

  # Three groups with interleaved keys
  result <- engine$eval_in_env(
    engine$read("(stable-sort (list (list 3 'a) (list 1 'b) (list 2 'c)
                                    (list 1 'd) (list 3 'e) (list 2 'f))
                   (lambda (x y) (< (car x) (car y))))")[[1]], env)
  # Group 1: b then d (original order)
  expect_equal(result[[1]], list(1, quote(b)))
  expect_equal(result[[2]], list(1, quote(d)))
  # Group 2: c then f (original order)
  expect_equal(result[[3]], list(2, quote(c)))
  expect_equal(result[[4]], list(2, quote(f)))
  # Group 3: a then e (original order)
  expect_equal(result[[5]], list(3, quote(a)))
  expect_equal(result[[6]], list(3, quote(e)))

  # Odd-length list (exercises uneven split)
  result <- engine$eval_in_env(
    engine$read("(stable-sort '(5 3 1 4 2) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3, 4, 5))
})

test_that("merge combines two sorted lists", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("sort"), env)

  result <- engine$eval_in_env(
    engine$read("(merge '(1 3 5) '(2 4 6) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3, 4, 5, 6))

  # One empty list
  result <- engine$eval_in_env(
    engine$read("(merge '() '(1 2 3) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3))

  # Both empty
  result <- engine$eval_in_env(
    engine$read("(merge '() '() <)")[[1]], env)
  expect_equal(result, list())

  # Other side empty
  result <- engine$eval_in_env(
    engine$read("(merge '(1 2 3) '() <)")[[1]], env)
  expect_equal(result, list(1, 2, 3))

  # Interleaved
  result <- engine$eval_in_env(
    engine$read("(merge '(1 4 7) '(2 5 8) <)")[[1]], env)
  expect_equal(result, list(1, 2, 4, 5, 7, 8))

  # One list entirely before the other
  result <- engine$eval_in_env(
    engine$read("(merge '(1 2 3) '(4 5 6) <)")[[1]], env)
  expect_equal(result, list(1, 2, 3, 4, 5, 6))

  # Duplicates across lists (stability: equal elements from list1 come first)
  result <- engine$eval_in_env(
    engine$read("(merge (list (list 1 'L1) (list 3 'L1))
                        (list (list 1 'L2) (list 3 'L2))
                   (lambda (x y) (< (car x) (car y))))")[[1]], env)
  expect_equal(result[[1]], list(1, quote(L1)))
  expect_equal(result[[2]], list(1, quote(L2)))
  expect_equal(result[[3]], list(3, quote(L1)))
  expect_equal(result[[4]], list(3, quote(L2)))

  # Single-element lists
  result <- engine$eval_in_env(
    engine$read("(merge '(1) '(2) <)")[[1]], env)
  expect_equal(result, list(1, 2))
})
