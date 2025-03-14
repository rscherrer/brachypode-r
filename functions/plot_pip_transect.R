# Function to plot a PIP transect
plot_pip_transect <- function(

  par, pos, vals, lab, x, model, pars, init = c(1, 1), tend = 100,
  twostep = TRUE, passon = FALSE, tol = 0.001, extra = TRUE, no_na = TRUE,
  cpp = FALSE

) {
  
  # par: parameter to vary
  # pos: position in the parameter list
  # vals: values to try
  # lab: prefix in facet labels
  # x: trait values to explore
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # passon: whether to treat starting points as known equilibria
  # tol: tolerance for invasion boundaries
  # extra: whether to add plots of demographic equilibria and selection gradients
  # no_na: whether to remove NAs from the PIPs
  
  # For each parameter value...
  data <- tibble(value = vals) %>%
    mutate(pip = map(value, function(value) {

      # Display progress
      print(paste(par, "=", value))

      # Update parameter value
      pars[[pos]] <- parse_expr(paste(par, "<-", value))

      # Make PIP
      plot_pip(x, model, pars, init, tend, twostep, passon, extra = extra, no_na = no_na, plotit = FALSE, cpp = cpp)

    })) %>%
    unnest(pip)

  # Relabel
  data <- data %>% add_labels("value", lab)

  # Plot the PIP
  p1 <- data %>%
    PLOTPIP(tol) +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  # Early exit if needed
  if (!extra) return(p1)

  # Make a plot of equilibrium densities
  p2 <- data %>%
    group_by(xres, value) %>%
    filter(x == all_of(x)[1]) %>%
    PLOTDENS() +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  # Make a plot of selection gradients
  p3 <- data %>%
    group_by(xres, value) %>%
    filter(seq(n()) == 1) %>%
    PLOTGRAD() +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  # Combine
  P <- wrap_plots(
    p3 + rm_axis("x"),
    p2 + rm_strips("x") + rm_axis("x"),
    p1 + rm_strips("x"),
    ncol = 1, heights = c(1, 1, 3)
  )

  # Return
  return(P)

}
