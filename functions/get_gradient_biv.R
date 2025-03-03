get_gradient_biv <- function(

  x, y, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
  fast = FALSE

) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  if (fast) N <- init else N <- find_equilibrium_biv(x, y, model, pars, init, tend, twostep, cpp)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  return(c(G1, G2))

}
