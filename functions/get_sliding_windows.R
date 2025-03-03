get_sliding_windows <- function(from, to, n, size, intervals = TRUE) {

  # Sequence of breaks
  breaks <- seq(from, to, length.out = n + 1)

  # Pick the starts and ends of each window
  starts <- breaks[1:(length(breaks) - size)]
  ends <- breaks[(1 + size):length(breaks)]

  # Return a list of intervals
  if (intervals) return(purrr::map2(starts, ends, ~ c(.x, .y)))

  # Or return start and end points separated
  return(list(starts = starts, ends = ends))

}
