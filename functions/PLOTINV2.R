# Function to plot invasion fitness heatmap
PLOTINV2 <- function(data) {
  
  # data: a data frame
  
  # Plot
  data %>% 
    ggplot(aes(x = x, y = y, fill = lambda)) +
    geom_tile() +
    labs(fill = parse(text = "lambda(x,y,hat(x),hat(y))")) +
    scale_fill_continuous(type = "viridis", option = "magma")
  
}