# Function to find multivariate root while catching errors
multiroot_noerr <- function(f, start, ..., as_na = FALSE, val_only = TRUE) {
  
  # f: the function to find the root of
  # start: starting conditions
  # ...: arguments for the root solver
  # as_na: whether to return errors as NAs instead of NULL
  # val_only: whether to only return the root and not the associated output
  
  # Try to...
  tryCatch({
    
    # Find a multivariate root
    root <- rootSolve::multiroot(f, start, ...)
    
    # Return the relevant output
    if (val_only) return(root$root) else return(root)
    
  # Otherwise catch the error
  }, error = function(err) { if (as_na) return(NA) else return(NULL) }
  
  )
  
}
