multiroot_noerr <- function(f, start, ..., as_na = FALSE, val_only = TRUE) {
  
  tryCatch({
    
    root <- rootSolve::multiroot(f, start, ...)
    if (val_only) return(root$root) else return(root)
    
  }, error = function(err) { if (as_na) return(NA) else return(NULL) }
  
  )
  
}
