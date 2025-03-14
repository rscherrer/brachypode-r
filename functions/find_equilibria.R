# Function to search demographic equilibria across trait values
find_equilibria <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE

) {

  # x: trait values to explore
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for equilibrium search
  
  # For each trait value...
  map_dfr(x, function(x) {

    # Look for demographic equilibrium
    N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)
    return(tibble(N1 = N[1], N2 = N[2]))

  }) %>%
    add_column(x = xvalues, .before = "N1")

}
