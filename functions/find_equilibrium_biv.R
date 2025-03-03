find_equilibrium_biv <- function(x, y, model, pars, init = c(1, 1), tend = 100, twostep = TRUE) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  if (tend > 0) {

    if (cpp) {

      N <- iterate_cpp(init, tend, x, y, get_par_values(pars))

    } else {

      # Simulate and hope we get to equilibrium
      data <- iterate_biv(model, pars, x, y, tend, N0 = init)
      N1 <- last(data$N1)
      N2 <- last(data$N2)
      N <- c(N1, N2)

    }

    if (twostep) N <- find_equilibrium_biv(x, y, model, pars, init = N, tend = 0, twostep = FALSE)

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
