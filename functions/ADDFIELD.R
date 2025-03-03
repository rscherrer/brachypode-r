ADDFIELD <- function(plot, data, scale = 0.1) {
  
  plot + 
    geom_point(data = data, size = 0.5, color = "black") +
    geom_segment(
      data = data,
      mapping = aes(xend = x1 + scale * G1, yend = x2 + scale * G2),
      color = "black"
    )
  
}