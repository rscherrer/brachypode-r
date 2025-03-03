plot_pip_biv <- function(

  x, y, xres, yres, model, pars, init = c(1, 1), tend = 100,
  twostep = TRUE, plotit = TRUE, binary = TRUE, tol = 0.001, cpp = FALSE,
  fast = FALSE

) {

  N <- init

  if (!fast) N <- find_equilibrium_biv(xres, yres, model, pars, init, tend, twostep, cpp)

  data <- expand_grid(x = x, y = y) %>%
    mutate(lambda = map2_dbl(
      x, y, get_lambda_biv, xres, yres, model, pars, init = N, fast = TRUE
    ))

  if (!plotit) return(data)

  if (binary) return(data %>% PLOTPIP2(tol))

  data %>% PLOTINV2()

}
