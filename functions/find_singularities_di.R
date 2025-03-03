find_singularities_di <- function(

  model, pars, xstart = c(1, 1), init = rep(1, 4), tend = 100, twostep = TRUE,
  cpp = FALSE, noerror = TRUE, val_only = TRUE, ...

) {

  # Equation for which to find the root(s)
  FUN <- function(x) get_gradient_di(x[1], x[2], model, pars, init, tend, twostep, cpp)

  # Find the roots of the selection gradient
  if (noerror) return(multiroot_noerr(FUN, start = xstart, ...))
  
  root <- rootSolve::multiroot(FUN, start = xstart, ...)
  if (val_only) return(root$root) else return(root)

}
