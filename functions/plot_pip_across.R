# Function to plot PIPs across a custom list of parameter combinations
plot_pip_across <- function(
    
  x, model, pars, init = c(1, 1), tend = 100,
  twostep = TRUE, passon = FALSE, tol = 0.001, no_na = TRUE,
  cpp = FALSE, ..., labels = NULL, labels2 = NULL
  
) {
  
  # TODO: Allow other ways to label
  
  # x: trait values to explore
  # model: model specifications
  # pars: a list of parameter settings
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # passon: whether to treat starting points as known equilibria
  # tol: tolerance for invasion boundaries
  # no_na: whether to remove NAs from the PIPs
  # ...: extra arguments for facetting
  # labels: custom labels for facets
  # labels2: second row of custom facet labels
  
  # For each parameter combination...
  pips <- map2(pars, seq(pars), function(pars, i) {
    
    # Display progress
    print(paste("Parameter set no.", i))
    
    # Generate PIP data
    plot_pip(x, model, pars, init, tend, twostep, passon, extra = FALSE, no_na = no_na, plotit = FALSE, cpp = cpp)
    
  })
  
  # Assemble in a table
  data <- tibble(data = pips, set = seq(pips))
  
  # Relabel
  data <- data %>% add_labels("set", "'Set no.", " ")
  
  # Override labels if needed
  if (!is.null(labels)) data$set_lab <- factor(labels, levels = labels) 
  
  # If needed...
  if (!is.null(labels2)) {
    
    # Check 
    if (is.null(labels))
      stop("First row of labels must be provided before second row")
    
    # Add a second row of labels
    data$set_lab2 <- factor(labels2, levels = labels2) 
    
  }
  
  # Unnest
  data <- data %>% unnest(data) 
  
  # Facet formula
  f <- ". ~ set_lab"
  if (!is.null(labels2)) f <- paste(f, "+ set_lab2")
  f <- as.formula(f)
  
  # Plot the PIP
  plot <- data %>%
    PLOTPIP(tol) +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_wrap(f, labeller = label_parsed, ...)
  
  return(plot)
  
}