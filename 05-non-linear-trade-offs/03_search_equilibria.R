## Here we plot the outcomes of equilibrium search across parameter space, but
## for different non-linear trade-off curves.

rm(list = ls())

library(tidyverse)
library(rlang)

theme_set(theme_classic())

source("../functions.R")

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

# Plot equilibria acrss parameter space
plot <- plot_param_space(
  par = c("theta2", "epsilon", "K2", "c"), 
  pos = c(7, 2, 4, 8),
  vals = list(
    theta2 = c(0, 1, 2, 3, 5),
    epsilon = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
    K2 = c(1800, 1000, 500, 100),
    c = c(0.1, 0.3, 0.5, 0.7, 0.9)
  ),
  lab = c("theta[UF]", "epsilon", "K[UF]", "c"),
  model = model2(),
  pars = pars,
  from = 0,
  to = 10,
  tend = 1000,
  n = 20
)

# Relabel
plot <- plot +
  xlab(parse(text = "'Stress in unfacilitated patches ('*theta[UF]*')'")) +
  ylab(parse(text = "'Trade-off ('*epsilon*')'"))

# Save
ggsave("plots/equilibrium_search_non_linear_trade_offs.png", plot, width = 5.4, height = 4, dpi = 300)
