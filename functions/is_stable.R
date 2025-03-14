# Function to tell if a singularity is evolutionarily stable
is_stable <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  value = FALSE, fast = FALSE, type = 1, dodge = 0.001, tol = 0.0001,
  cpp = FALSE

) {
  
  # x: trait value
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # value: whether to return the value of the fitness curvature instead of yes or no
  # fast: whether to treat starting points as known equilibria
  # type: 1 for sign of fitness curvature or 2 for mutual invasibility
  # dodge: perturbation used in mutual invasibility in the phenotypic vicinity  
  # tol: tolerance used in mutual invasibility

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # If needed...
  if (fast) {

    # Treat starting points as known equilibria 
    N <- init

  } else {

    # Otherwise search demographic equilibrium
    N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)

  }

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  # If needed...
  if (type == 2) {

    # Perturb the singularity a little 
    x1 <- x - dodge
    x2 <- x + dodge

    # Check that the perturbed trait values can mutually invaded each other
    return(!is_mut_inv(x1, x2, model, pars, init, tend, twostep, tol, cpp))

  }

  # Return fitness curvature if needed
  if (value) return(H)

  # Otherwise the singularity is stable if the curvature is negative
  H < 0

}
