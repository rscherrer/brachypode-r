# Function to plot selection gradients across trait values
PLOTGRAD <- function(data) {
  
  # data: a data frame
  
  # Setup to pick colors
  ii <- 1:2
  if (!any(data$G < 0)) ii <- 1
  if (!any(data$G > 0)) ii <- 2
  colors <- c("red", "blue")[ii]
  
  # Plot
  data %>%
    ggplot(aes(x = xres, y = G)) +
    ggh4x::stat_difference(aes(ymin = 0, ymax = G), alpha = 0.5, levels = c("> 0", "< 0", NA), na.rm = TRUE) +
    geom_line() +
    xlab(parse(text = "'Resident trait value ('*hat(x)*')'")) +
    ylab("Gradient") +
    scale_fill_manual(values = colors, na.translate = FALSE) +
    labs(fill = parse(text = "G(hat(x))"))
  
}