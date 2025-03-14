# Function to catch errors when root finding
uniroot_noerr <- function(f, interval, ..., as_na = FALSE, val_only = FALSE) {

  # f: the function to find roots for
  # interval: from where to where?
  # ...: extra arguments to be passed to the root finding function
  # as_na: return errors as NAs instead of NULL
  # val_only: whether to only return the root value (or the output of the root finder)
  
  # Try to...
  tryCatch({
    
    # Find the root of the function
    root <- uniroot(f, interval, ...)
    
    # Return the output
    if (val_only) return(root$root) else return(root)
    
    # Catch errors in the meantime
    }, error = function(err) { if (as_na) return(NA) else return(NULL) }

  )

}
