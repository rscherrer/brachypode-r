# Function to compute a bivariate selection gradient
get_gradient_biv <- function(

  x, y, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
  fast = FALSE

) {
  
  # x, y: trait values
  # model: bivariate model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # fast: whether to treat starting points as known equilibria

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium
  if (fast) N <- init else N <- find_equilibrium_biv(x, y, model, pars, init, tend, twostep, cpp)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  # Return bivariate selection gradient
  return(c(G1, G2))

}
