## Here is a function to find a bivariate equilibrium strategy ({x1, x2} such
## that the bivariate gradient is zero), but it does not work so well. Plus, 
## I have not found a convenient way to make it find multiple equilibria, so I
## have not added it to plot_mip.

rm(list = ls())

library(tidyverse)
library(rlang)

source("../functions.R")

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.5,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5,
  xmax <- 10
  
)

# Test the function
find_singularities_di(
  model = model_di2(), pars = pars, xstart = c(3, 7), init = rep(1, 4), 
  tend = 500, twostep = TRUE
)