# Function to compute invasion fitness 
get_lambda <- function(

  x, xres, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  tol = 0.0001, cpp = FALSE, fast = FALSE

) {
  
  # x: mutant trait value
  # xres: resident trait value
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # tol: tolerance for closeness of demographic equilibrium to zero
  # fast: whether starting points should be treated as known equilibria (avoids searching) 
  
  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Setup
  N <- init

  # Search demographic equilibria if needed
  if (!fast) N <- find_equilibrium(xres, model, pars, init, tend, twostep, cpp)

  # Check for extinction
  if (any(N < -tol) | all(N < tol)) return(NA)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  # Return invasion fitness
  return(lambda)

}
