## Here we explore dynamics in two demes with various levels of migration and
## differences in shrub cover.

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
  K2 <- 500,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  deltac <- 0,
  c1 <- 0.5 - deltac / 2,
  c2 <- 0.5 + deltac / 2,
  m <- 0.01

)

# Generate many PIPs
pips <- plot_pip_grid(
  par1 = "deltac",
  par2 = "m",
  pos1 = 8,
  pos2 = 11,
  vals1 = c(0, 0.2, 0.4, 0.6, 0.8, 0.98),
  vals2 = c(0.00001, 0.0001, 0.001, 0.01, 0.1),
  lab1 = "Delta*c",
  lab2 = "m",
  x = seq(0, 10, 0.1),
  model = model_twosites(),
  pars = pars,
  init = rep(1, 4)
)

# Save
ggsave("plots/pips_two_sites.png", pips, width = 8, height = 8, dpi = 300)

# Generate many MIPs
mips <- plot_mip_grid(
  par1 = "deltac",
  par2 = "m",
  pos1 = 8,
  pos2 = 11,
  vals1 = c(0, 0.2, 0.4, 0.6, 0.8, 0.98),
  vals2 = c(0.00001, 0.0001, 0.001, 0.01, 0.1),
  lab1 = "Delta*c",
  lab2 = "m",
  x = seq(0, 10, 0.1),
  model = model_twosites(),
  pars = pars,
  init = rep(1, 4)
)

# Save
ggsave("plots/mips_two_sites.png", mips, width = 8, height = 8, dpi = 300)
