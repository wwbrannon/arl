library(testthat)
library(arl)

# Treat warnings as errors so unexpected warnings fail the test suite
old_warn <- options(warn = 2)
on.exit(options(old_warn), add = TRUE)

test_check("arl")
