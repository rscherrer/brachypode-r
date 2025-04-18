## Here we plot some PIPs across some parameter values.

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
plot <- plot_pip_grid(
  par1 = "K2",
  par2 = "theta2",
  pos1 = 4,
  pos2 = 7,
  vals1 = c(2000, 1800, 1000, 500, 100),
  vals2 = c(0, 1, 2, 3, 5),
  lab1 = "K[UF]",
  lab2 = "theta[UF]",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_carrying_capacity_by_environmental_stress.png", plot, width = 8, height = 6, dpi = 300)
