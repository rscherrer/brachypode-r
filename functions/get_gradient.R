get_gradient <- function(

    x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
    fast = FALSE

) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  if (fast) N <- init else N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  return(G)

}
