make_cran_thinner <- function(keep_every = 3L) {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    return(function() invisible(NULL))
  }
  counter <- 0L
  function() {
    counter <<- counter + 1L
    if (counter %% keep_every != 1L) {
      skip("CRAN test thinning")
    }
  }
}
