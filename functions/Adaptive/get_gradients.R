# Function to compute selection gradients across resident trait values
get_gradients <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
  fast = FALSE

) {
  
  # x: vector of trait values
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp, fast: arguments for gradient computation 
  
  # Set up initial conditions
  if (!fast) init <- map(x, ~ init)

  # Compute gradient across trait values
  tibble(
    x = x,
    G = map2_dbl(x, init, ~ get_gradient(.x, model, pars, .y, tend, twostep, cpp, fast))
  )

}
