PLOTDENS <- function(data) {

  n <- length(data$N[[1]])

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
