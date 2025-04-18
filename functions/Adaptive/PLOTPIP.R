# Function to display a pairwise invasibility plot
PLOTPIP <- function(data, tol = 0.001, no_na = TRUE) {

  # data: a data frame
  # tol: tolerance for closeness of fitness to the invasion boundary
  # no_na: whether to remove NAs
    
  # Remove NAs if needed
  if (no_na) data <- data %>% drop_na()
  
  # With the data...
  data <- data %>%
    mutate(
      
      # Round the fitness values
      lambda = round(lambda / tol) * tol,
      
      # Establish where the mutant invades
      class = if_else(lambda < 1, "< 1", if_else(lambda > 1, "> 1", "= 1")),
      class = factor(class, levels = c("< 1", "= 1", "> 1"))
    ) 
  
  # Plot
  data <- data %>%
    ggplot(aes(x = xres, y = x, fill = class)) +
    geom_tile() +
    xlab(parse(text = "'Resident trait value ('*hat(x)*')'")) +
    ylab(parse(text = "'Mutant trait value ('*x*')'")) +
    labs(fill = parse(text = "lambda(x,hat(x))")) +
    scale_fill_manual(values = c("gray20", "gray50", "gray80"))
  
}
