PLOTINV <- function(data) {
  
  data %>%
    ggplot(aes(x = xres, y = x, fill = lambda)) +
    geom_tile() +
    xlab(parse(text = "'Resident trait value ('*hat(x)*')'")) +
    ylab(parse(text = "'Mutant trait value ('*x*')'")) +
    labs(fill = parse(text = "lambda(x,hat(x))")) +
    scale_fill_continuous(type = "viridis", option = "magma")
  
}