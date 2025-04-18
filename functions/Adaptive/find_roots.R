# Function to find the roots of a function with one argument using sliding windows
find_roots <- function(f, from, to, n = 10, size = 1, precis = NULL, ...) {

  # f: the function
  # from, to: bounds of search space
  # n, size: arguments for sliding windows
  # precis: precision to use to remove duplicate solutions
  # ...: extra arguments for the root finding function
  
  # Early exit if needed
  if (size > n) stop("size must be smaller or equal to n")

  # Sliding windows
  windows <- get_sliding_windows(from, to, n, size)

  # For each window...
  roots <- purrr::map(windows, function(win) {

    # Find a root with a wrapper to catch errors
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
