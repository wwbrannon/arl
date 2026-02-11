required <- c("devtools", "rmarkdown", "knitr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       ". Install with install.packages(...).")
}

devtools::document()
rmarkdown::render("README.Rmd", quiet = TRUE)
devtools::build_vignettes()

source(file.path("tools", "cran", "cran_check.R"), local = TRUE)
