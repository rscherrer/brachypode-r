# Function to add fitness isoclines to a mutual invasibility plot
ADDCLINES <- function(plot, gdata1, gdata2, col1 = "blue", col2 = "red") {
  
  # plot: the plot
  # gdata1, gdata2: isocline coordinates along both dimensions
  # col1, col2: colors of the two isoclines
  
  # Plot
  plot +
    geom_point(data = gdata1, aes(fill = NULL), color = col1, show.legend = FALSE) +
    geom_point(data = gdata2, aes(fill = NULL), color = col2, show.legend = FALSE)
  
}