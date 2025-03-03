PLOTPAR <- function(data, lab = NULL) {
  
  data <- data %>%
    drop_na() %>%
    filter(CS > 0 | BP > 0) %>%
    mutate(group = paste(CS, "CSS", BP, "BP"))
  
  if (!is.null(lab)) {
    
    data <- data %>%
      add_labels("val3", lab[3]) %>%
      add_labels("val4", lab[4])
    
  } else {
    
    data <- data %>%
      mutate(val3_lab = val3, val4_lab = val4)
    
  }
  
  plot <- data %>%
    ggplot(aes(x = factor(val1), y = factor(val2), fill = group)) +
    geom_tile() +
    facet_grid(val3_lab ~ val4_lab, labeller = label_parsed) +
    scale_fill_manual(values = c("lightsalmon", "lightblue", "salmon3", "steelblue")) +
    labs(fill = NULL)
  
  if (!is.null(lab)) {
    
    plot <- plot +
      xlab(parse(text = lab[1])) +
      ylab(parse(text = lab[2]))
    
  }
  
  return(plot)
  
}