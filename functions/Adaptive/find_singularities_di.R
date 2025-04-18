# Function to find dimorphic singularities
find_singularities_di <- function(

  model, pars, xstart = c(1, 1), init = rep(1, 4), tend = 100, twostep = TRUE,
  cpp = FALSE, noerror = TRUE, val_only = TRUE, ...

) {

  # model: dimorphic model specifications
  # pars: parameter values
  # xstart: starting conditions for singularity search
  # init: starting conditions for demographic equilibrium search
  # tend, twostep, cpp: arguments for demographic equilibrium search
  # noerror: whether to use the error catching wrapper function
  # val_only: whether to return just the root and not associated output
  # ...: extra arguments for the root finding function
  
  # Equation for which to find the root(s)
  FUN <- function(x) get_gradient_di(x[1], x[2], model, pars, init, tend, twostep, cpp)

  # Find the roots of the selection gradient without error if needed
  if (noerror) return(multiroot_noerr(FUN, start = xstart, ...))
  
  # Otherwise use the non-wrapped version
  root <- rootSolve::multiroot(FUN, start = xstart, ...)
  if (val_only) return(root$root) else return(root)

}
