# Compute the densities at the next generation
get_nextgen <- function(model, pars, x, N) {
  
  # model: the demographic model
  # pars: parameter values
  # x: trait value
  # N: vector of densities (must correspond to the model)
  
  # Evaluate the parameters
  for (i in seq(pars)) eval(pars[[i]])
  
  # Evaluate the model given the parameters
  for (i in seq(model)) eval(model[[i]])
  
  # Compute the next generation
  c(Lambda %*% N)
  
}
