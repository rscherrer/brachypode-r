is_convergent <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = FALSE,
  dx = 0.001, value = FALSE, cpp = FALSE

) {

  g0 <- get_gradient(x, model, pars, init, tend, twostep, cpp)
  g1 <- get_gradient(x - dx, model, pars, init, tend, twostep, cpp)
  g2 <- get_gradient(x + dx, model, pars, init, tend, twostep, cpp)

  g1 <- g1 - g0
  g2 <- g2 - g0

  C <- (g2 - g1) / (2 * dx)

  if (value) return(C) else return(C < 0)

}
