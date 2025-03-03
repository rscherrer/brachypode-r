get_lambda_biv <- function(

  x, y, xres, yres, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  tol = 0.0001, cpp = FALSE, fast = FALSE

) {

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  N <- init

  # If the demographic equilibrium is not given...
  if (!fast) N <- find_equilibrium_biv(xres, yres, model, pars, init, tend, twostep, cpp)

  if (any(N < -tol) | all(N < tol)) return(NA)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  return(lambda)

}
