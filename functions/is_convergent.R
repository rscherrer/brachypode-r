# Function to tell whether a singularity is convergent stable
is_convergent <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = FALSE,
  dx = 0.001, value = FALSE, cpp = FALSE

) {
  
  # x: trait value
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # dx: perturbation amplitude
  # value: whether to return the value of the convergence criterion instead of yes or no

  # Compute selection gradient around the singularity
  g0 <- get_gradient(x, model, pars, init, tend, twostep, cpp)
  g1 <- get_gradient(x - dx, model, pars, init, tend, twostep, cpp)
  g2 <- get_gradient(x + dx, model, pars, init, tend, twostep, cpp)

  # Compute differences
  g1 <- g1 - g0
  g2 <- g2 - g0

  # Compute convergence criterion
  C <- (g2 - g1) / (2 * dx)

  # The singularity is convergent if the criterion is negative
  if (value) return(C) else return(C < 0)

}
