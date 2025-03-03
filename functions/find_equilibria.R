find_equilibria <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE

) {

  map_dfr(x, function(x) {

    N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)
    return(tibble(N1 = N[1], N2 = N[2]))

  }) %>%
    add_column(x = xvalues, .before = "N1")

}
