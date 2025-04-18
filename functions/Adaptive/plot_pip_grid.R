# Function to produce a grid of PIPs
plot_pip_grid <- function(

  par1, par2, pos1, pos2, vals1, vals2, lab1, lab2,
  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  tol = 0.001, no_na = TRUE, cpp = FALSE

) {
  
  # par1, par2: names of the parameters to vary
  # pos1, pos2: positions of these parameters in the parameter list
  # vals1, vals2: values to explore for both
  # lab1, lab2: label prefix to use in plot facets
  # x, model, pars, init, tend, twostep, passon, tol, cpp: arguments for the PIP function 
  # no_na: whether to remove NAs from the output

  # For each combination of parameters...
  data <- expand_grid(

    val1 = vals1,
    val2 = vals2

  ) %>%
    mutate(pip = pmap(list(val1, val2, seq(n())), function(val1, val2, i) {

      # Display
      print(paste(i, "/", n()))

      # Update parameter values
      pars[[pos1]] <- parse_expr(paste(par1, "<-", val1))
      pars[[pos2]] <- parse_expr(paste(par2, "<-", val2))

      # Generate a PIP
      plot_pip(x, model, pars, init, tend, twostep, passon, tol = tol, extra = FALSE, plotit = FALSE, cpp = cpp)

    })) %>%
    unnest(pip)

  # Prettify
  data <- data %>%
    rename(par1 = "val1", par2 = "val2") %>%
    add_labels("par1", lab1) %>%
    add_labels("par2", lab2)

  # Plot
  plot <- data %>%
    PLOTPIP(tol) +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(par1_lab ~ par2_lab, labeller = label_parsed)

  return(plot)

}
