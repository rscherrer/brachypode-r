find_equilibrium_di <- function(x1, x2, model, pars, init = rep(1, 4), tend = 100, twostep = TRUE, cpp = FALSE) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  if (tend > 0) {

    if (cpp) {

      N <- iterate_di_cpp(init, tend, c(x1, x2), get_par_values(pars))

    } else {

      # Simulate and hope we get to equilibrium
      data <- iterate_di(model, pars, x1, x2, tend, N1 = init[1:2], N2 = init[3:4])
      N11 <- last(data$N11)
      N12 <- last(data$N12)
      N21 <- last(data$N21)
      N22 <- last(data$N22)
      N <- c(N11, N12, N21, N22)

    }

    if (twostep) N <- find_equilibrium_di(x1, x2, model, pars, init = N, tend = 0, twostep = FALSE)

    return(N)

  }

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
