# Function to make a mutual invasibility plot
PLOTMIP <- function(data) {
  
  # data: a data frame
  
  # Plot
  data %>%
    ggplot(aes(x = x1, y = x2, fill = protected)) +
    geom_tile() +
    geom_abline() +
    scale_fill_manual(values = c("gray20", "gray80")) +
    xlab(parse(text = "'Trait of morph 1 ('*x[1]*')'")) +
    ylab(parse(text = "'Trait of morph 2 ('*x[2]*')'")) +
    labs(fill = "Mut. Inv.")
  
}