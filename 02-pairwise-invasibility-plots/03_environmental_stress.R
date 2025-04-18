## Here we explore the effect of increasing environmental stress.

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

# Make a series of PIPs
P <- plot_pip_transect(
  par = "theta2",
  pos = 7,
  vals = c(0, 1, 3, 5),
  lab = "theta[UF]",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_environmental_stress.png", P, width = 7, height = 4, dpi = 300)

