# Topological sort for directed acyclic graphs (DAGs).
# Reusable for dependency order, build order, etc.
# Used by StdlibDeps for stdlib module load order.

#' Topological sort of a DAG
#'
#' Returns vertices in an order such that for every edge (from -> to), \code{to}
#' appears before \code{from}. Useful for dependency order, build order, etc.
#'
#' @param vertices Character vector of vertex names.
#' @param edges List of edges. Each element is a list with \code{from} and
#'   \code{to} (character, single element). Edge \code{from -> to} means
#'   "from depends on to"; the returned order has \code{to} before \code{from}.
#' @return Character vector of vertex names in topological order.
#' @keywords internal
#' @noRd
rye_topsort <- function(vertices, edges) {
  if (!is.character(vertices)) {
    stop("vertices must be a character vector")
  }
  vertices <- unique(vertices)
  # Build adjacency: adj[[v]] = character vector of nodes v depends on (v -> u means v imports u)
  adj <- list()
  for (v in vertices) adj[[v]] <- character()
  for (e in edges) {
    from <- e$from
    to <- e$to
    if (is.null(from) || is.null(to)) next
    if (length(from) != 1L || length(to) != 1L) next
    from <- as.character(from)[1L]
    to <- as.character(to)[1L]
    if (from %in% vertices && to %in% vertices && !to %in% adj[[from]]) {
      adj[[from]] <- c(adj[[from]], to)
    }
  }
  # in_degree[v] = number of dependencies of v (must be loaded before v)
  in_degree <- stats::setNames(rep(0L, length(vertices)), vertices)
  for (v in vertices) in_degree[[v]] <- length(adj[[v]])
  # who_depends_on[u] = nodes that have u as a dependency
  who_depends_on <- list()
  for (v in vertices) who_depends_on[[v]] <- character()
  for (v in vertices)
    for (u in adj[[v]])
      who_depends_on[[u]] <- c(who_depends_on[[u]], v)
  queue <- vertices[in_degree == 0L]
  order <- character()
  while (length(queue) > 0L) {
    u <- queue[1L]
    queue <- queue[-1L]
    order <- c(order, u)
    for (v in who_depends_on[[u]]) {
      in_degree[[v]] <- in_degree[[v]] - 1L
      if (in_degree[[v]] == 0L) queue <- c(queue, v)
    }
  }
  if (length(order) != length(vertices)) {
    stop("Cycle detected in dependency graph")
  }
  order
}
