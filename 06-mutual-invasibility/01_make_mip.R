## Here we generate a mutual invasibility plot for a particular combination
## of parameters.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)

# C++ stuff
sourceCpp("../functions/Adaptive/src/iterate.cpp")
sourceCpp("../functions/Adaptive/src/iterate_di.cpp")

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

# Make a MIP
mip <- plot_mip(
  seq(0, 10, 0.1), model(), pars, 
  field = seq(0, 10, 0.25), 
  model_di = model_di(), 
  grid = seq(0, 10, 0.2), 
  lower = TRUE, cpp = TRUE, scale = 0.2
)

# Note: the MIP is generated with arrow field showing the dimorphic selection
# gradient, and with isoclines. We are also using C++ code to speed up the
# iterative computation of demographic equilibria.

# Save it
saveRDS(mip, "data/mip.rds")