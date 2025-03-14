# Function to compute a dimorphic selection gradient
get_gradient_di <- function(

  x1, x2, model, pars, init = rep(1, 4), tend = 100, twostep = TRUE,
  cpp = FALSE, fast = FALSE

) {
  
  # x1, x2: pair of trait values
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographc equilibrium search
  # fast: NOT USED HERE

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  N <- find_equilibrium_di(x1, x2, model, pars, init, tend, twostep, cpp)

  # Separate population sizes for the two species
  N1 <- N[1:2]
  N2 <- N[3:4]

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  # Return the pair of selection gradients
  return(c(G1, G2))

}
