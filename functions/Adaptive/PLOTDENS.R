# Function to plot equilibrium densities across trait values
PLOTDENS <- function(data) {

  # data: a data frame
  
  # Number of colors
  n <- length(data$N[[1]])

  # Plot
  data %>%
    unnest(N) %>%
    mutate(name = paste0("N[", seq(n), "]")) %>%
    ggplot(aes(x = xres, y = N, color = name)) +
    geom_line() +
    xlab(parse(text = "'Resident trait value ('*hat(x)*')'")) +
    ylab("Density") +
    labs(color = NULL) +
    scale_color_manual(
      values = scales::hue_pal()(n),
      labels = function(x) parse(text = x)
    )
}
