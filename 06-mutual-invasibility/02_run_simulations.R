## Here we run a few simulations with two morphs that can initially coexist, 
## to see where this dimorphic coalition evolves.

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

# Load the MIP
mip <- readRDS("data/mip.rds")

set.seed(42) # for reproducibility

# Pick a number of simulations to run
nsim <- 5L

# For a given number of simulations to run...
simdata <- mip$data %>%
  filter(protected, x2 > x1) %>%
  filter(seq(n()) %in% sample(n(), nsim)) %>%
  mutate(data = pmap(list(x1, x2, seq(n())), function(x1, x2, i) {
    
    # Display
    print(paste(i, "/", n()))
    
    # Run the simulation (with C++ code)
    simulate(
      model(), pars, c(x1, x2), init = rep(1, 4), ntimes = 1000, tend = 1000,
      sigma = 0.5, model_di = model_di(), verbose = FALSE, cpp = TRUE
    )
    
  }))

# Save the simulated data
saveRDS(simdata, "data/simdata.rds")