find_equilibrium <- function(x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  if (tend > 0) {

    if (cpp) {

      N <- iterate_cpp(init, tend, x, get_par_values(pars))

    } else {

      # Simulate and hope we get to equilibrium
      data <- iterate(model, pars, x, tend, N0 = init)
      N <- unlist(data[nrow(data), -1])

    }

    if (twostep) N <- find_equilibrium(x, model, pars, init = N, tend = 0, twostep = FALSE)

    return(N)

  }

  # System of equations to solve
  f <- function(N) {

    for (i in seq(model)) eval(model[[i]])
    return(N - Lambda %*% N)

  }

  # Solve the demographic equilibrium
  N <- pracma::fsolve(f, init)$x

  return(N)

}
