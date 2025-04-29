## Here we explore PIPs across different rates of migration and dispersal between
## the patches (just for eyeballing, we do not save figures).

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

source("../functions.R")

# Parameters
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 500,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5,
  phi <- 0
  
)

# Make a series of PIPs
plot_pip_transect(
  par = "phi",
  pos = 9,
  vals = c(0, 0.5, 0.9),
  lab = "phi",
  seq(0, 10, 0.1), 
  model3(), 
  pars,
  extra = FALSE
)

# For different values of local dispersal...
data <- map_dfr(c(0, 0.5, 0.9), function(phi) {
  
  # Update parameter value
  pars[[9]] <- parse_expr(paste("phi <-", phi))
  
  # Simulate the demographic dynamics
  iterate(model3(), pars, x = 6) %>%
    mutate(phi = phi)
  
}, .id = "sim")

# Plot
data %>%
  pivot_longer(N1:N2) %>%
  ggplot(aes(x = t, y = value, linetype = name)) +
  geom_line() +
  facet_grid(. ~ phi)

# New parameter values
pars2 <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5,
  m <- 0.5
  
)

# Produce PIPs across migration rates
plot_pip_transect(
  par = "m",
  pos = 9,
  vals = c(0.1, 0.3, 0.5),
  lab = "m",
  seq(0, 10, 0.1), 
  model4(), 
  pars2,
  extra = FALSE
)
