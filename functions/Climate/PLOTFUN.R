# Function to plot the results
PLOTFUN <- function(
    
  dir = "../data/climate-change/", tmax = 40000, plot_traits = FALSE, show_titles = TRUE, 
  show_y = TRUE, show_xlab = TRUE, ymax = 300, rm_legend = TRUE
  
) {
  
  # dir: the directory where to find all the sets of simulations
  # tmax: end of the simulation
  # plot_traits: whether to plot traits instead of densities
  # show_titles: whether to show the titles of each plot
  # show_y: whether to show the y-axis to the left
  # show_xlab: whether to show the title of the x-axis at the bottom 
  # ymax: the limit of the y-axis
  
  # List of sets of simulations (one per climate change scenario)
  sets <- str_c(dir, c(
    "stress-increase-K2-100", "landscape-deterioration", 
    "cover-shrinkage-K2-100", "macro-mutations"
  ))
  
  # For each set...
  plots <- map(sets, function(set) {
    
    # Progress
    print(str_c("Plotting set ", set))
    
    # For each simulation within that set...
    data <- map_dfr(list.dirs(set)[-1], function(dir) {
      
      # Read the parameters
      pars <- read_parameters(dir)
      
      # Pick the right reading function
      this_read <- if (plot_traits) read_individual_data else read_patch_size_data
      
      # Read the simulation data and append key parameters
      this_read(dir) %>%
        mutate(
          twarming = pars$twarming,
          pgoodEnd = pars$pgoodEnd,
          capacitiesEnd2 = pars$capacitiesEnd[2],
          stressEnd2 = pars$stressEnd[2],
          capacities1 = pars$capacities[1]
        )
      
    }, .id = "sim")
    
    # Add labels
    data <- data %>% add_labels("twarming", "Delta*t[W]")
    
    # Figure how long each population survived
    tdata <- data %>%
      group_by(twarming_lab) %>%
      summarize(tend = max(time))
    
    # Set the plot up
    plot <- data %>%
      mutate(patch = if_else(patch == 0, "UF", "F")) %>%
      ggplot(aes(x = time / 1000, y = get(ifelse(plot_traits, "x", "n")))) 
    
    # Add layer depending on what we are plotting
    plot <- if (plot_traits) plot + geom_point(aes(color = factor(patch))) else plot + geom_line(aes(color = factor(patch)))
    
    # Prepare label
    ylab <- ifelse(plot_traits, "Stress tolerance (x)", "No. individuals")
    
    # Rest of the plot
    plot <- plot + facet_wrap(. ~ twarming_lab, labeller = label_parsed) +
      scale_color_manual(values = c("gray20", "gray80")) +
      xlab(parse(text = "'Time ('*10^3~'generations)'")) +
      labs(color = "Patch") +
      xlim(c(0, tmax / 1000)) +
      ylim(c(0, ymax)) +
      ylab(ylab) +
      geom_vline(xintercept = 10, linetype = 4) +
      geom_rect(aes(xmin = tend / 1000, x = NULL, y = NULL), data = tdata, xmax = tmax / 1000, ymin = 0, ymax = ymax, fill = "gray20", alpha = 0.5)
    
    # Remove y-axis if needed
    if (!show_y) plot <- plot + rm_axis("y")
    
    return(plot)
    
  })
  
  # Update names
  names(plots) <- str_remove(sets, "../data/climate-change/")
  
  # Update facets and labels
  plots <- map(plots, ~ .x + facet_grid(. ~ twarming_lab, labeller = label_parsed))
  
  # Prepare titles
  titles <- get_scenario_names()
  
  # Empty titles if needed
  if (!show_titles) titles <- rep("", length(titles))
  
  # Add titles
  plots <- map2(plots, titles, ~ .x + ggtitle(.y))
  
  # Further customization
  plots[2:4] <- map(plots[2:4], ~ .x + rm_strips("x"))
  plots[1:3] <- map(plots[1:3], ~ .x + rm_axis("x"))
  if (rm_legend) plots <- map(plots, ~ .x + theme(legend.position = "none"))
  plots <- map(plots, ~ .x + scale_x_continuous(breaks = seq(0, tmax / 1000, length.out = 5)))
  
  # Combine
  plot <- wrap_plots(plots, ncol = 1)
  
  # Remove x-axis if needed (should only affect the last plot)
  if (!show_xlab) plot <- plot + xlab(NULL)
  
  return(plot)
  
}
