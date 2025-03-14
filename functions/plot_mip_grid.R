# Function to generate a grid of MIPs
plot_mip_grid <- function(

  par1, par2, pos1, pos2, vals1, vals2, lab1, lab2,
  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  passon = FALSE, tol = 0.001, field = NULL, model_di = NULL, scale = 0.1,
  grid = NULL, verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue",
  col2 = "red", cpp = FALSE

) {
  
  # par1, par2: the parameters to vary
  # pos1, pos2: positions of those parameters in the parameter list
  # vals1, vals2: values to try
  # lab1, lab2: prefix of facet labels
  # x: trait values to explore
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # passon, tol, field, model_di, scale, grid, verbose, refined, lower: arguments for MIP plotting
  # col1, col2: arguments for cline plotting
  
  # For each combination of trait values...
  data <- expand_grid(

    val1 = vals1,
    val2 = vals2

  ) %>%
    mutate(mip = pmap(list(val1, val2, seq(n())), function(val1, val2, i) {

      # Display progress
      print(paste(i, "/", n()))

      # Update parameter values
      pars[[pos1]] <- parse_expr(paste(par1, "<-", val1))
      pars[[pos2]] <- parse_expr(paste(par2, "<-", val2))

      # Make a MIP
      plot_mip(
        x, model, pars, init, tend, twostep, passon, tol, field, model_di,
        scale, grid, verbose, refined, lower, plotit = FALSE, as_list = TRUE,
        cpp = cpp
      )

    }))

  # Relabel
  data <- data %>%
    rename(par1 = "val1", par2 = "val2") %>%
    add_labels("par1", lab1) %>%
    add_labels("par2", lab2)

  # Internal function to extract list elements 
  EXTRACT <- function(data, which) {

    # data: the data
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
    facet_grid(par1_lab ~ par2_lab, labeller = label_parsed)

  # If needed...
  if (!is.null(field)) {

    # Extract the selection gradient fields 
    fdata <- data %>% EXTRACT("field")
    
    # Add to plot
    plot <- plot %>% ADDFIELD(fdata, scale)

  }

  # If needed...
  if (!is.null(grid)) {

    # Extract fitness isocline data
    gdata1 <- data %>% EXTRACT("clines1")
    gdata2 <- data %>% EXTRACT("clines2")

    # Add them to the plot
    plot <- plot %>% ADDCLINES(gdata1, gdata2, col1, col2)

  }

  # Return plot
  return(plot)

}
