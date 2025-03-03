uniroot_noerr <- function(f, interval, ..., as_na = FALSE, val_only = FALSE) {

  tryCatch({
    
    root <- uniroot(f, interval, ...)
    if (val_only) return(root$root) else return(root)
    
    }, error = function(err) { if (as_na) return(NA) else return(NULL) }

  )

}
