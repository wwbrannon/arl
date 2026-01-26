test_that("rye_trimws_compat trims whitespace", {
  input <- c("  hello  ", "\tgoodbye", "see ya\n")
  expect_equal(rye:::rye_trimws_compat(input), c("hello", "goodbye", "see ya"))
  expect_equal(rye:::rye_trimws_compat(input, which = "left"), c("hello  ", "goodbye", "see ya\n"))
  expect_equal(rye:::rye_trimws_compat(input, which = "right"), c("  hello", "\tgoodbye", "see ya"))
})

test_that("rye_trimws_shim matches base trimws when available", {
  if (!exists("trimws", mode = "function", inherits = TRUE)) {
    skip("base trimws not available")
  }
  input <- c("  hello  ", "\tgoodbye", "see ya\n")
  expect_equal(rye:::rye_trimws_shim(input), trimws(input))
  expect_equal(rye:::rye_trimws_shim(input, which = "left"), trimws(input, which = "left"))
  expect_equal(rye:::rye_trimws_shim(input, which = "right"), trimws(input, which = "right"))
})
