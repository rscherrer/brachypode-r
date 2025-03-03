# Here we perform a fitness set analysis, where we basically look at how the 
# fitness isocline crosses the trade-off line between stress tolerance and
# reproductive output.

rm(list = ls())

library(tidyverse)
library(rlang)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 200,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5
  
)

# Set the seed
set.seed(54)

# Plot fitness sets
plots <- plot_fitness_sets(
  seq(0, 10, 1), 
  model(),
  model_biv(),
  pars, 
  scalex = 0.08, 
  scaley = 0.08, 
  n = 200, 
  L = 0.25
)

# Save
ggsave("plots/fitness_sets.png", plots[[1]], width = 4, height = 3, dpi = 300)
ggsave("plots/bivariate_pips.png", plots[[2]], width = 6, height = 7, dpi = 300)
