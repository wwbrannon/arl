# Tests for rye_topsort (R/topological-sort.R)

test_that("simple DAG returns valid order", {
  vertices <- c("a", "b", "c")
  edges <- list(
    list(from = "b", to = "a"),
    list(from = "c", to = "b")
  )
  order <- rye:::rye_topsort(vertices, edges)
  expect_type(order, "character")
  expect_setequal(order, vertices)
  pos <- setNames(seq_along(order), order)
  expect_lt(pos[["a"]], pos[["b"]])
  expect_lt(pos[["b"]], pos[["c"]])
})

test_that("multiple components are all included", {
  vertices <- c("a", "b", "x", "y")
  edges <- list(
    list(from = "b", to = "a"),
    list(from = "y", to = "x")
  )
  order <- rye:::rye_topsort(vertices, edges)
  expect_setequal(order, vertices)
  expect_length(order, 4L)
  pos <- setNames(seq_along(order), order)
  expect_lt(pos[["a"]], pos[["b"]])
  expect_lt(pos[["x"]], pos[["y"]])
})

test_that("cycle is detected and errors", {
  vertices <- c("a", "b", "c")
  edges <- list(
    list(from = "b", to = "a"),
    list(from = "c", to = "b"),
    list(from = "a", to = "c")
  )
  expect_error(rye:::rye_topsort(vertices, edges), "Cycle detected")
})

test_that("empty vertices returns character(0)", {
  expect_identical(rye:::rye_topsort(character(0), list()), character(0))
})

test_that("single node returns that node", {
  expect_identical(rye:::rye_topsort("x", list()), "x")
})

test_that("edges only among vertices are respected", {
  vertices <- c("a", "b")
  edges <- list(
    list(from = "b", to = "a"),
    list(from = "b", to = "z")
  )
  order <- rye:::rye_topsort(vertices, edges)
  expect_setequal(order, c("a", "b"))
  expect_lt(match("a", order), match("b", order))
})
