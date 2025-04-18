## Here we look at the effect of increasing the trade-off parameter.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

source("../functions.R")

# Parameter values
pars <- alist(

  rmax <- 2,
  epsilon <- 0,
  K1 <- 2000,
  K2 <- 2000,
  a <- 5,
  theta1 <- 0,
  theta2 <- 0,
  c <- 0.5

)

# Make PIPs
P <- plot_pip_transect(
  par = "epsilon",
  pos = 2,
  vals = c(0, 0.1, 0.2, 0.4),
  lab = "epsilon",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_trade_off.png", P, width = 7, height = 3.5, dpi = 300)
