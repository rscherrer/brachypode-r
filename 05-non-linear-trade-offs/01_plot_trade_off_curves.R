# Here we plot various non-linear trade-off curves.

rm(list = ls())

library(tidyverse)
library(rlang)

theme_set(theme_classic())

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5,
  xmax <- 10
  
)

# For several trade-off curves and trait values...
plot <- expand_grid(
  
  x = seq(0, 10, 0.1),
  epsilon = seq(0.1, 0.9, 0.1)
  
) %>%
  mutate(y = map2_dbl(x, epsilon, function(x, epsilon) {
    
    # Update parameters accordingly
    pars[[2]] <- parse_expr(paste("epsilon <-", epsilon))
    
    # Overwrite previous parameter values
    for (i in seq(pars)) eval(pars[[i]])
    
    # Measure special quantity
    d <- -log2(1 - epsilon)
    
    # Compute reproductive output
    return(rmax * ((1 - (x / xmax)^(1 / d))^d))
    
  })) %>%
  
  # And plot the curves
  ggplot(aes(x = x, y = y, color = epsilon, group = epsilon)) +
  geom_line() +
  scale_color_gradient(low = "white", high = "black") +
  xlab("Stress tolerance (x)") +
  ylab("Reproductive output (y)") +
  labs(color = parse(text = "epsilon"))

# Save
ggsave("plots/trade_off_curves.png", plot, width = 4, height = 3, dpi = 300)