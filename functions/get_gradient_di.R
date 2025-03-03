get_gradient_di <- function(

  x1, x2, model, pars, init = rep(1, 4), tend = 100, twostep = TRUE,
  cpp = FALSE, fast = FALSE

) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_di(x1, x2, model, pars, init, tend, twostep, cpp)

  # Separate population sizes for the two species
  N1 <- N[1:2]
  N2 <- N[3:4]

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  return(c(G1, G2))

}
