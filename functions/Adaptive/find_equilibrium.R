# Function to find a demographic equilibrium
find_equilibrium <- function(x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE) {

  # x: trait value of the resident population
  # model: model describing the population dynamics
  # pars: parameter values
  # init: initial conditions for the equilibrium search
  # tend: maximum simulation time (in iterations, set to zero to directly use root finding)
  # twostep: whether to run root finding after simulation
  # cpp: whether to use C++ code to run simulations
  
  # Note: pick a reasonable number of time steps to increase the chances that
  # you get to an equilibrium if using a simulation.
  
  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # If simulation...
  if (tend > 0) {

    # If in C++...
    if (cpp) {

      # Simulate demographics using the C++ source code
      N <- iterate_cpp(init, tend, x, get_par_values(pars))

    } else {

      # Otherwise use R code
      data <- iterate(model, pars, x, tend, N0 = init)
      N <- unlist(data[nrow(data), -1])

    }

    # Root finding with simulation outcome as starting point if needed
    if (twostep) N <- find_equilibrium(x, model, pars, init = N, tend = 0, twostep = FALSE)

    return(N)

  }
  
  # Note: the following only runs if no simulation is to be run. 

  # System of equations to solve
  f <- function(N) {

    # Evaluate all the equations in the model
    for (i in seq(model)) eval(model[[i]])
    
    # Transition matrix
    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium using root finding
  N <- pracma::fsolve(f, init)$x

  return(N)

}
