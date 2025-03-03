# Function to find the roots of a function with one argument
find_roots <- function(f, from, to, n = 10, size = 1, precis = NULL, ...) {

  if (size > n) stop("size must be smaller or equal to n")

  # Sliding windows
  windows <- get_sliding_windows(from, to, n, size)

  # Look for roots in multiple windows
  roots <- purrr::map(windows, function(win) {

    uniroot_noerr(f, interval = win, ...)

  })

  # Remove nulls
  roots <- roots[!purrr::map_lgl(roots, is.null)]

  # Simplify
  roots <- purrr::map_dbl(roots, ~ .x[["root"]])

  # Remove duplicates (at a certain level of precision)
  if (!is.null(precis)) roots <- round(roots, precis)
  roots[!duplicated(roots)]

}
