# Here we plot MIPs across trade-off and stress levels.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)

# C++ stuff
sourceCpp("../src/iterate.cpp")
sourceCpp("../src/iterate_di.cpp")

theme_set(theme_classic())

# Load functions
for (f in list.files("../functions", full.names = TRUE)) source(f)

# Parameters
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.3
  
)

# Plot MIPs across values of trade-off and stress levels
mips <- plot_mip_grid(
  par1 = "epsilon",
  par2 = "theta2", 
  pos1 = 2, 
  pos2 = 7, 
  vals1 = c(0.001, 0.01, 0.1, 0.2, 0.4),
  vals2 = c(0, 1, 2, 3, 5),
  lab1 = "epsilon",
  lab2 = "theta[UF]", 
  x = seq(0, 10, 0.1),
  model = model(), 
  pars = pars,
  cpp = TRUE
)

# Save
ggsave(
  "plots/mips_environmental_stress_by_trade_off.png", 
  mips + scale_x_continuous(breaks = c(0, 5, 10)), 
  width = 6, height = 5, dpi = 300
)
