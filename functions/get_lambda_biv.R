# Function to compute bivariate invasion fitness
get_lambda_biv <- function(

  x, y, xres, yres, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  tol = 0.0001, cpp = FALSE, fast = FALSE

) {
  
  # x, y: mutant trait values
  # xres, yres: resident trait values
  # model: bivariate model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # tol: tolerance to tell whether fitness is equal to zero
  # fast: whether to treat starting points as known equilibria

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Setup
  N <- init

  # If the demographic equilibrium is not given...
  if (!fast) N <- find_equilibrium_biv(xres, yres, model, pars, init, tend, twostep, cpp)

  if (any(N < -tol) | all(N < tol)) return(NA)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  return(lambda)

}
