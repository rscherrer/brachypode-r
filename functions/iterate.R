# Iterate the demographic dynamics through time
iterate <- function(model, pars, x, tend = 100, N0 = c(1, 1)) {

  ncol <- length(N0)

  # Setup
  M <- matrix(0, ncol = ncol, nrow = tend + 1)
  M[1,] <- N0

  # Initialization
  N <- N0

  # For each time step...
  for (t in seq(tend)) {

    # Compute the densities of the next generation
    N <- get_nextgen(model, pars, x, N)
    M[t + 1,] <- N

  }

  # Formatting
  M <- as_tibble(M)
  names(M) <- paste0("N", seq(ncol))
  M <- M %>% mutate(t = 0:tend)
  M <- M[, c("t", paste0("N", seq(ncol)))]

  return(M)

}
