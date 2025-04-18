## Here we look at the effect of increasing asymmetry in carrying capacity.

rm(list = ls())

library(tidyverse)
library(patchwork)
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
  theta2 <- 0,
  c <- 0.5

)

# Make PIPs
P <- plot_pip_transect(
  par = "K2",
  pos = 4,
  vals = c(2000, 1950, 1900, 1000, 500),
  lab = "K[UF]",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_carrying_capacity.png", P, width = 9, height = 4, dpi = 300)

