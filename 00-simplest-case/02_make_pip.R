# Here we make a PIP, trying different ways of computing the demographic
# equilibrium.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

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

# Make PIP based on root-finding
plot <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1000, 1000), twostep = FALSE, extra = TRUE)

# Save
ggsave("plots/simplest_case_pip.png", plot, width = 5, height = 6, dpi = 300)

# Make PIP based on simulations
plot <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1, 1), tend = 100, twostep = FALSE, extra = TRUE)

# Save
ggsave("plots/simplest_case_pip_sim.png", plot, width = 5, height = 6, dpi = 300)
