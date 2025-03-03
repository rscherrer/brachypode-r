get_gradients <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
  fast = FALSE

) {

  if (!fast) init <- map(x, ~ init)

  tibble(
    x = x,
    G = map2_dbl(x, init, ~ get_gradient(.x, model, pars, .y, tend, twostep, cpp, fast))
  )

}
