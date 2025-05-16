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
  epsilon <- 0.4,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5,
  xmax <- 10,
  nu <- 1
  
)

# Plot equilibria acrss parameter space
plot <- plot_param_space(
  par = c("theta2", "nu", "K2", "c"), 
  pos = c(7, 10, 4, 8),
  vals = list(
    theta2 = c(0, 1, 2, 3, 5),
    nu = c(0.25, 0.5, 1, 2, 4),
    K2 = c(1800, 1000, 500, 100),
    c = c(0.1, 0.3, 0.5, 0.7, 0.9)
  ),
  lab = c("theta[UF]", "nu", "K[UF]", "c"),
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
  ylab(parse(text = "'Trade-off non-linearity ('*nu*')'"))

# Save
ggsave("plots/equilibrium_search_non_linear_trade_offs.png", plot, width = 5.4, height = 4, dpi = 300)
