# Function to plot a heatmap of singularities across parameter space
PLOTPAR <- function(data, lab = NULL) {
  
  # data: a data frame
  # lab: prefix for facet labels
  
  # Count convergent stable strategies and branching points
  data <- data %>%
    drop_na() %>%
    filter(CS > 0 | BP > 0) %>%
    mutate(group = paste(CS, "CSS", BP, "BP"))
  
  # If needed...
  if (!is.null(lab)) {
    
    # Prepare facet labels
    data <- data %>%
      add_labels("val3", lab[3]) %>%
      add_labels("val4", lab[4])
    
  } else {
    
    # Otherwise default labels
    data <- data %>%
      mutate(val3_lab = val3, val4_lab = val4)
    
  }
  
  # Plot
  plot <- data %>%
    ggplot(aes(x = factor(val1), y = factor(val2), fill = group)) +
    geom_tile() +
    facet_grid(val3_lab ~ val4_lab, labeller = label_parsed) +
    scale_fill_manual(values = c("lightsalmon", "lightblue", "salmon3", "steelblue")) +
    labs(fill = NULL)
  
  # If needed...
  if (!is.null(lab)) {
    
    # Rename the axes
    plot <- plot +
      xlab(parse(text = lab[1])) +
      ylab(parse(text = lab[2]))
    
  }
  
  # Return plot
  return(plot)
  
}