# Iterate the demographic dynamics through time
iterate_di <- function(model, pars, x1, x2, tend = 100, N1 = c(1, 1), N2 = c(1, 1)) {
  
  # model: the model
  # pars: parameter values
  # x1, x2: pair of trait values
  # tend: number of iterations
  # N1, N2: vectors of densities for both morphs (must correspond to the model)
  
  # Setup
  M <- matrix(0, ncol = 4, nrow = tend + 1)
  M[1,] <- c(N1, N2)
  
  # For each time step...
  for (t in seq(tend)) {
    
    # Compute the densities of the next generation
    N <- get_nextgen_di(model, pars, x1, x2, N1, N2)
    M[t + 1,] <- N
    
    # Prepare for next round
    N1 <- N[1:2]
    N2 <- N[3:4]
    
  }
  
  # Formatting
  M <- as_tibble(M)
  names(M) <- c("N11", "N12", "N21", "N22")
  M <- M %>% mutate(t = 0:tend) %>% select(t, N11, N12, N21, N22)
  
  return(M)
  
}
