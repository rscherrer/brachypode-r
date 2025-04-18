# Function to generate a MIP transect
plot_mip_transect <- function(

  par, pos, vals, lab, x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  passon = FALSE, tol = 0.001, field = NULL, model_di = NULL, scale = 0.1,
  grid = NULL, verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue",
  col2 = "red", cpp = FALSE

) {
  
  # par: name of the parameter to vary 
  # pos: position in the parameter list
  # vals: values to explore
  # lab: prefix in facet labels
  # x: trait values to explore
  # model: dimorphic model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # passon, tol, field, model_di, scale, grid, verbose, refined, lower: arguments for MIP plotting
  # col1, col2: arguments for cline plotting
  
  # For each value of the parameter to try... 
  data <- tibble(value = vals) %>%
    mutate(mip = map(value, function(value) {

      # Display progress
      print(paste(par, "=", value))

      # Update parameter value
      pars[[pos]] <- parse_expr(paste(par, "<-", value))

      # Make MIP
      plot_mip(
        x, model, pars, init, tend, twostep, passon, tol, field, model_di,
        scale, grid, verbose, refined, lower, plotit = FALSE, as_list = TRUE,
        cpp = cpp
      )

    }))

  # Relabel
  data <- data %>% add_labels("value", lab)

  # Function to extract specific list elements
  EXTRACT <- function(data, which) {
    
    # data: the list
    # which: which element

    # Extract
    data %>%
      mutate(.extr = map(mip, ~ .x[[which]])) %>%
      select(-mip) %>%
      unnest(.extr)

  }

  # Plot the MIPs
  plot <- data %>%
    EXTRACT("data") %>%
    PLOTMIP() +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  # If needed...
  if (!is.null(field)) {

    # Extract selection gradient field data
    fdata <- data %>% EXTRACT("field")
    
    # Add to plot
    plot <- plot %>% ADDFIELD(fdata, scale)

  }

  # If needed...
  if (!is.null(grid)) {

    # Extract fitness isoclines
    gdata1 <- data %>% EXTRACT("clines1")
    gdata2 <- data %>% EXTRACT("clines2")

    # Add to plot
    plot <- plot %>% ADDCLINES(gdata1, gdata2, col1, col2)

  }

  # Return plot
  return(plot)

}
