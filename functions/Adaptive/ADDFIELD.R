# Function to add a field of selection gradient tick marks to a plot
ADDFIELD <- function(plot, data, scale = 0.1) {
  
  # plot: the plot
  # data: a data frame containing the tick mark coordinates
  # scale: scaling parameter for the length of the tick marks
  
  # Plot
  plot + 
    geom_point(data = data, size = 0.5, color = "black") +
    geom_segment(
      data = data,
      mapping = aes(xend = x1 + scale * G1, yend = x2 + scale * G2),
      color = "black"
    )
  
}