# Function to find fitness isoclines in a dimorphic system
find_clines <- function(

    x, model_di, pars, i = 1, j = 1, init = c(1, 1), tend = 100, twostep = TRUE,
    verbose = TRUE, refined = TRUE, model = NULL, tol = 0.001, toprint = "x",
    cpp = FALSE

) {
  
  # x: trait values to explore
  # model_di: the dimorphic model
  # pars: parameter values
  # i: which of the two dimensions do we want to calculate along? 
  # j: orientation of the clines for plotting (which dimension?)
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # verbose: whether to display progress 
  # refined: whether to double check the results with a monomorphic version
  # model: monomorphic model (NULL if not needed)
  # tol: tolerance to use for mutual invasibility
  # toprint: prefix to use when displaying progress
  
  # Maximum trait value
  xmax <- max(x)

  # For each trait value...
  map_dfr(x, function(x) {

    # Display progress if needed
    if (verbose) print(paste(toprint, "=", x))

    # Prepare a function of the counterpart trait value...
    FUN <- function(y) {

      # If needed...
      if (refined) {

        # Check that the monomorphic model is provided 
        if (is.null(model)) stop("model must be supplied if refined is TRUE")

        # Check mutual invasibility
        if (!is_mut_inv(x, y, model, pars, init, tend, twostep, tol, cpp))
          return(42)

      }

      # Initialize the two trait values
      x1 <- ifelse(j == 1, x, y)
      x2 <- ifelse(j == 1, y, x)

      # Compute the dimorphic selection gradient
      G <- get_gradient_di(x1, x2, model_di, pars, rep(init, 2), tend, twostep, cpp)

      # Return the relevant component
      return(G[i])

    }

    # Set up bounds of search space
    from <- ifelse(j == 1, x, 0)
    to <- ifelse(j == 1, xmax, x)
    
    # Find the roots of the function
    y <- find_roots(FUN, from, to)

    # Return in a certain order based on the orientation we want
    if (j == 1) return(tibble(x1 = x, x2 = y))

    # Otherwise return in the opposite order
    return(tibble(x1 = y, x2 = x))

  })

}
