# Function to add basins of attraction of equilibria to a MIP
ADDBASINS <- function(plot, basins, i = 1) {

  # plot: the MIP
  # basins: the basins of attraction
  # i: orientation (which diagonal to plot onto)

  # Nest the basin data
  tmp <- basins %>% group_by(xeq1, xeq2) %>% nest()

  # Keep the diagonal of interest
  data <- basins %>%
    filter(xeq1 == tmp$xeq1[i], xeq2 == tmp$xeq2[i]) %>%
    filter(x2 > x1)

  # Add to the plot
  plot +
    geom_point(data = data, aes(color = conv, fill = NULL)) +
    geom_point(data = data, aes(fill = NULL), shape = 21) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "yellow")) +
    labs(color = "Conv.")

}
