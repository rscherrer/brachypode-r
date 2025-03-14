# Compute the densities at the next generation
get_nextgen_di <- function(model, pars, x1, x2, N1, N2) {
  
  # model: the model
  # pars: parameter values
  # x1, x2: pair of trait values
  # N1, N2: vectors of densities for both morphs 
  
  # Evaluate the parameters
  for (i in seq(pars)) eval(pars[[i]])
  
  # Evaluate the model given the parameters
  for (i in seq(model)) eval(model[[i]])
  
  # Assemble the two transition matrices into one
  Zeros <- matrix(0, 2, 2)
  Lambda <- rbind(cbind(Lambda1, Zeros), cbind(Zeros, Lambda2))
  
  # Compute the next generation
  c(Lambda %*% c(N1, N2))
  
}
