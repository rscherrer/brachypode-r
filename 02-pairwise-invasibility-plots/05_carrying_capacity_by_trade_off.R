## Here we plot some PIPs across some parameter values.

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
  K2 <- 2000,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5

)

# Make the PIPs
plot <- plot_pip_grid(
  par1 = "K2",
  par2 = "epsilon",
  pos1 = 4,
  pos2 = 2,
  vals1 = c(2000, 1000, 100, 500, 100),
  vals2 = c(0, 0.001, 0.01, 0.1, 0.2, 0.4),
  lab1 = "K[UF]",
  lab2 = "epsilon",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_carrying_capacity_by_trade_off.png", plot, width = 10, height = 6, dpi = 300)
