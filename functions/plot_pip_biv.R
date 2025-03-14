# Function to produce a bivariate PIP
plot_pip_biv <- function(

  x, y, xres, yres, model, pars, init = c(1, 1), tend = 100,
  twostep = TRUE, plotit = TRUE, binary = TRUE, tol = 0.001, cpp = FALSE,
  fast = FALSE

) {
  
  # x, y: mutant trait values
  # xres, yres: resident trait values
  # model: bivariate model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # plotit: whether to return the plot (instead of the data)
  # binary: whether to show invasion reasons (instead of a fitness heatmap)
  # tol: tolerance to use for fitness equality
  # fast: whether to treat starting points as known equilibria

  # Set up
  N <- init

  # Compute bivariate equilibria if needed
  if (!fast) N <- find_equilibrium_biv(xres, yres, model, pars, init, tend, twostep, cpp)

  # Compute invasion fitness for each bivariate combination
  data <- expand_grid(x = x, y = y) %>%
    mutate(lambda = map2_dbl(
      x, y, get_lambda_biv, xres, yres, model, pars, init = N, fast = TRUE
    ))

  # Return the data if needed
  if (!plotit) return(data)

  # Generate PIP if needed
  if (binary) return(data %>% PLOTPIP2(tol))

  # Otherwise fitness heatmap
  data %>% PLOTINV2()

}
