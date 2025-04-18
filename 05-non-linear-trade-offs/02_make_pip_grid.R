## Here we generate PIPs across trade-off curves and carrying capacities.

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

# Generate a series of PIPs across trade-off curves
plot <- plot_pip_grid(
  par1 = "K2", 
  par2 = "epsilon", 
  pos1 = 4, 
  pos2 = 2, 
  vals1 = c(100, 500, 1000, 2000), 
  vals2 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  lab1 = "K[UF]", 
  lab2 = "epsilon", 
  model2(), 
  pars, 
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_non_linear_trade_offs.png", plot, width = 10, height = 6, dpi = 300)
