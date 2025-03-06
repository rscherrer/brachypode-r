# Here we are making a series of PIPs across values of environmental stress
# and shrub cover to understand what traps the population in an extinction
# vortex when the shrub cover goes down.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Load C++ code
sourceCpp("../src/iterate.cpp")

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
  K1 <- K1 + (K2 - K1) * theta1 / theta2
  
)

# Make a series of PIPs across parameter combinations
plot <- plot_pip_grid(
  par1 = "c",
  par2 = "theta1",
  pos1 = 8,
  pos2 = 6, 
  vals1 = c(0.1, 0.15, 0.18, 0.2, 0.3),
  vals2 = c(0, 1, 2, 3),
  lab1 = "c",
  lab2 = "theta[F]",
  x = seq(0, 10, 0.1), 
  model = model(), 
  pars,
  cpp = TRUE
)

# Save
ggsave("plots/climate_change_pips.png", plot, width = 7, height = 6.5, dpi = 300)