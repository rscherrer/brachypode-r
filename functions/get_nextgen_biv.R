# Compute the densities at the next generation
get_nextgen_biv <- function(model, pars, x, y, N) {
  
  # Evaluate the parameters
  for (i in seq(pars)) eval(pars[[i]])
  
  # Evaluate the model given the parameters
  for (i in seq(model)) eval(model[[i]])
  
  # Compute the next generation
  c(Lambda %*% N)
  
}
