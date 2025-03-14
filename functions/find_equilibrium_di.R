# Function to find dimorphic demographic equilibrium
find_equilibrium_di <- function(x1, x2, model, pars, init = rep(1, 4), tend = 100, twostep = TRUE, cpp = FALSE) {

  # x1, x2: pair of resident trait values
  # model: model specifications
  # pars: parameter values
  # init: starting points of the search
  # tend: number of iterations if using a simulation (set to zero for no simulation)
  # twostep: whether to use results of simulation as starting point in subsequent root finding
  # cpp: whether to run simulation with C++ code
  
  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # If simulation...
  if (tend > 0) {

    # If needed...
    if (cpp) {

      # Simulate using C++ code
      N <- iterate_di_cpp(init, tend, c(x1, x2), get_par_values(pars))

    } else {

      # Otherwise simply use R code
      data <- iterate_di(model, pars, x1, x2, tend, N1 = init[1:2], N2 = init[3:4])
      
      # Extract and reshape
      N11 <- last(data$N11)
      N12 <- last(data$N12)
      N21 <- last(data$N21)
      N22 <- last(data$N22)
      N <- c(N11, N12, N21, N22)

    }

    # Run subsequent root finding if needed
    if (twostep) N <- find_equilibrium_di(x1, x2, model, pars, init = N, tend = 0, twostep = FALSE)

    return(N)

  }
  
  # Note: we only get here if no simulation was run.

  # System of equations to solve
  f <- function(N) {

    # Decompose the vector of population sizes into the two morphs
    N1 <- N[1:2]
    N2 <- N[3:4]

    # Evaluate the model
    for (i in seq(model)) eval(model[[i]])

    # Assemble the two transition matrices into one
    Zeros <- matrix(0, 2, 2)
    Lambda <- rbind(cbind(Lambda1, Zeros), cbind(Zeros, Lambda2))

    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium
  N <- pracma::fsolve(f, init)$x

  return(N)

}
