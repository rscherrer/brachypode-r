find_clines <- function(

    x, model_di, pars, i = 1, j = 1, init = c(1, 1), tend = 100, twostep = TRUE,
    verbose = TRUE, refined = TRUE, model = NULL, tol = 0.001, toprint = "x",
    cpp = FALSE

) {

  xmax <- max(x)

  map_dfr(x, function(x) {

    if (verbose) print(paste(toprint, "=", x))

    FUN <- function(y) {

      if (refined) {

        if (is.null(model)) stop("model must be supplied if refined is TRUE")

        if (!is_mut_inv(x, y, model, pars, init, tend, twostep, tol, cpp))
          return(42)

      }

      x1 <- ifelse(j == 1, x, y)
      x2 <- ifelse(j == 1, y, x)

      G <- get_gradient_di(x1, x2, model_di, pars, rep(init, 2), tend, twostep, cpp)

      return(G[i])

    }

    from <- ifelse(j == 1, x, 0)
    to <- ifelse(j == 1, xmax, x)
    y <- find_roots(FUN, from, to)

    if (j == 1) return(tibble(x1 = x, x2 = y))

    return(tibble(x1 = y, x2 = x))

  })

}
