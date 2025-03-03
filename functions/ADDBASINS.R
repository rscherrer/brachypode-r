ADDBASINS <- function(plot, basins, i = 1) {
  
  tmp <- basins %>% group_by(xeq1, xeq2) %>% nest()
  
  data <- basins %>% 
    filter(xeq1 == tmp$xeq1[i], xeq2 == tmp$xeq2[i]) %>%
    filter(x2 > x1)
  
  plot +
    geom_point(data = data, aes(color = conv, fill = NULL)) +
    geom_point(data = data, aes(fill = NULL), shape = 21) +
    scale_color_manual(values = c("black", "yellow")) +
    labs(color = "Conv.")
  
}
