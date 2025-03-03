PLOTPIP2 <- function(data, tol = 0.001) {
  
  data %>% 
    mutate(
      
      lambda = round(lambda / tol) * tol,
      class = if_else(lambda < 1, "< 1", if_else(lambda > 1, "> 1", "= 1")),
      class = factor(class, levels = c("< 1", "= 1", "> 1"))
      
    ) %>%
    ggplot(aes(x = x, y = y)) +
    geom_tile(aes(fill = class)) +
    scale_fill_manual(values = c("gray20", "gray50", "gray80")) +
    labs(fill = parse(text = "lambda(x,y,hat(x),hat(y))"))
  
}