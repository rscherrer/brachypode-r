# Function to find a bivariate demographic equilibrium
find_equilibrium_biv <- function(x, y, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE) {

  # x, y: trait values
  # model: bivariate model specifications
  # parameters: parameter values
  # init: starting point for equilibrium search
  # tend: number of iterations for the simulation (set zero for no simulation)
  # twostep: whether to run root finding with simulation outcome as starting point
  # cpp: whether to use C++ code

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # If simulation...
  if (tend > 0) {

    # If needed...
    if (cpp) {

      # Run a simulation with C++ code
      N <- iterate_cpp(init, tend, x, y, get_par_values(pars))

    } else {

      # Otherwise use R code
      data <- iterate_biv(model, pars, x, y, tend, N0 = init)

      # Gather the results
      N1 <- last(data$N1)
      N2 <- last(data$N2)
      N <- c(N1, N2)

    }

    # Run root finding aferwards if needed
    if (twostep) N <- find_equilibrium_biv(x, y, model, pars, init = N, tend = 0, twostep = FALSE)

    return(N)

  }

  # Note: we get here if we set simulation time to zero.

  # System of equations to solve
  f <- function(N) {

    for (i in seq(model)) eval(model[[i]])
    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium
  N <- pracma::fsolve(f, init)$x

  return(N)

}
