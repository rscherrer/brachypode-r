ADDCLINES <- function(plot, gdata1, gdata2, col1 = "blue", col2 = "red") {
  
  plot +
    geom_point(data = gdata1, aes(fill = NULL), color = col1, show.legend = FALSE) +
    geom_point(data = gdata2, aes(fill = NULL), color = col2, show.legend = FALSE)
  
}