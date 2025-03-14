# Iterate the bivariate demographic dynamics through time
iterate_biv <- function(model, pars, x, y, tend = 100, N0 = c(1, 1)) {
  
  # model: model specifications
  # pars: parameter values
  # x, y: trait values
  # tend: number of iteration steps
  # N0: starting densities
  
  # Setup
  M <- matrix(0, ncol = 2, nrow = tend + 1)
  M[1,] <- N0
  
  # Initialization
  N <- N0
  
  # For each time step...
  for (t in seq(tend)) {
    
    # Compute the densities of the next generation
    N <- get_nextgen_biv(model, pars, x, y, N)
    M[t + 1,] <- N
    
  }
  
  # Formatting
  M <- as_tibble(M)
  names(M) <- c("N1", "N2")
  M <- M %>% mutate(t = 0:tend) %>% select(t, N1, N2)
  
  return(M)
  
}
