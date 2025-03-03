PLOTPIP <- function(data, tol = 0.001, no_na = TRUE) {
  
  if (no_na) data <- data %>% drop_na()
  
  data %>%
    mutate(
      lambda = round(lambda / tol) * tol,
      class = if_else(lambda < 1, "< 1", if_else(lambda > 1, "> 1", "= 1")),
      class = factor(class, levels = c("< 1", "= 1", "> 1"))
    ) %>%
    ggplot(aes(x = xres, y = x, fill = class)) +
    geom_tile() +
    xlab(parse(text = "'Resident trait value ('*hat(x)*')'")) +
    ylab(parse(text = "'Mutant trait value ('*x*')'")) +
    labs(fill = parse(text = "lambda(x,hat(x))")) +
    scale_fill_manual(values = c("gray20", "gray50", "gray80"))
  
}
