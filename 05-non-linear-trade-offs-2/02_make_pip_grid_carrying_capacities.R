## Here we generate PIPs across carrying capacities for different
## trade-off curves.

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

# Generate a series of PIPs across trade-off curves
plot <- plot_pip_grid(
  par1 = "K2", 
  par2 = "nu", 
  pos1 = 4, 
  pos2 = 10, 
  vals1 = c(100, 500, 1000, 2000), 
  vals2 = c(0.25, 0.5, 1, 2, 4),
  lab1 = "K[UF]", 
  lab2 = "nu", 
  model2(), 
  pars, 
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_carrying_capacity_vs_non_linear_trade_offs.png", plot, width = 8, height = 6, dpi = 300)
