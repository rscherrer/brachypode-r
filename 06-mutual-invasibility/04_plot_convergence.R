## Here we map the area of dimorphic trait space that converges towards a dimorphic
## equilibrium, and we plot it on top of the MIP.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)

# C++ stuff
sourceCpp("../functions/Adaptive/src/iterate.cpp")
sourceCpp("../functions/Adaptive/src/iterate_di.cpp")

theme_set(theme_classic())

# Load functions
source("../functions.R")

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

# Find dimorphic equilibria
find_singularities_di(model_di(), pars, xstart = c(2, 1.5), cpp = TRUE, noerror = TRUE)

# Note: Root finding is not very good at finding biologically relevant dimorphic
# singularities. What I could do instead is use the isoclines and isolate the 
# pairs of points that are close enough.

# Compute basins of attraction
basins <- plot_mip(
  seq(0, 10, 0.1), model(), pars,
  model_di = model_di(), 
  field = seq(0, 10, 0.25),
  grid = seq(0, 10, 0.2), 
  cpp = TRUE,
  plotit = FALSE
) %>% get_basins(precis = 2)

# Re-plot the MIP, this time with isoclines showing on the upper diagonal
mip <- plot_mip(
  seq(0, 10, 0.1), model(), pars,
  model_di = model_di(), 
  field = seq(0, 10, 0.25),
  scale = 0.2,
  grid = seq(0, 10, 0.2), 
  cpp = TRUE
)

# Add the basins of attraction of the first singularity identified
mip <- mip %>% ADDBASINS(basins, i = 1)

# Save
ggsave("plots/mip_example.png", mip, width = 5, height = 4, dpi = 300)