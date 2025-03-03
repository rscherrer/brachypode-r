simulate <- function(

  model, pars, x, ntimes = 100, init = c(1, 1), tend = 100, twostep = TRUE,
  mu = 0.01, sigma = 0.1, passon = FALSE, extinct = 0.01, model_di = NULL,
  tol = 0.001, branch = 1, dodge = 0.01, tol_lambda = 0.0001, verbose = TRUE,
  cpp = FALSE

) {

  # Number of morphs
  k <- length(x)

  # Mono- or dimorphic allowed
  if (k > 2) stop("x should be of length 1 or 2.")

  # Set-up
  if (k == 1) x <- c(x, NA)

  # Initialize vectors of trait values through time
  x1s <- x2s <- rep(NA, ntimes)

  # Initialize time
  t <- 0

  # For each time step...
  while (t < ntimes) {

    # Increment time
    t <- t + 1

    # Verbose if needed
    if (verbose) {

      print(paste0("t = ", t, ", x1 = ", x[1], ", x2 = ", x[2]))

    }

    # Error if needed
    if (k == 2 & is.null(model_di)) {

      stop("model_di must be supplied for dimorphic evolution.")

    }

    # Record trait values
    x1s[t] <- x[1]
    x2s[t] <- x[2]

    # Compute equilibrium densities
    if (k == 1) {

      # Monomorphic
      N <- find_equilibrium(x[!is.na(x)], model, pars, init, tend, twostep, cpp)

    } else {

      # Or dimorphic
      N <- find_equilibrium_di(x[1], x[2], model_di, pars, init, tend, twostep, cpp)

    }

    # Stop the simulation if the whole population is extinct
    if (all(N < extinct)) break

    # Floor to zero
    N[N < 0] <- 0

    # Set the next starting conditions to current predicted densities if needed
    if (passon) init <- N

    # If partial extinction of a morph when dimorphic...
    if (k == 2) {

      # Revert to monomorphic trait, densities and initial conditions
      ii <- map_lgl(list(1:2, 3:4), ~ all(N[.x] < extinct))

      N <- N[rep(!ii, each = 2)]
      init <- init[rep(!ii, each = 2)]
      x[ii] <- NA

      if (any(is.na(x))) k <- 1

    }

    # Compute selection gradient
    if (k == 1) {

      # Monomorphic
      G <- get_gradient(x[!is.na(x)], model, pars, init = N, fast = TRUE)

    } else {

      # Dimorphic
      G <- get_gradient_di(x[1], x[2], model_di, pars, init = N, fast = TRUE)

    }

    # If monomorphic and branching happens...
    if (branch > 0 & k == 1 & G[1] > -tol & G[1] < tol) {

      stable <- is_stable(x[!is.na(x)], model, pars, init = N, fast = TRUE, type = branch, dodge = dodge, tol = tol_lambda)

      if (!stable) {

        # Dodge trait and duplicate initial conditions
        x[1] <- x[!is.na(x)] - dodge
        x[2] <- x[!is.na(x)] + dodge
        init <- c(init, init)

        if (passon) init <- init / 2

        k <- 2

      }

    } else {

      # Compute total population size of each morph
      Ntot <- N[1] + N[2]
      if (k == 2) Ntot <- c(Ntot, N[3] + N[4])

      # Update trait values based on the selection gradient
      x <- x + 0.5 * Ntot * sigma^2 * mu * G

    }
  }

  times <- seq(t)

  return(tibble(time = times, x1 = x1s[times], x2 = x2s[times]))

}
