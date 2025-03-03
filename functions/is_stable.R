is_stable <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  value = FALSE, fast = FALSE, type = 1, dodge = 0.001, tol = 0.0001,
  cpp = FALSE

) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  if (fast) {

    N <- init

  } else {

    N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)

  }

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  if (type == 2) {

    x1 <- x - dodge
    x2 <- x + dodge

    return(!is_mut_inv(x1, x2, model, pars, init, tend, twostep, tol, cpp))

  }

  if (value) return(H)

  # Is the equilibrium stable?
  H < 0

}
