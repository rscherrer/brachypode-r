# Here we generate a series of PIPs showing how branching points break down as
# we modify the asymmetry in carrying capacity between the two habitat patches. 

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5
  
)

# Generate a grid of PIPs
plot <- plot_pip_grid(
  par1 = "K1",
  par2 = "K2",
  pos1 = 3,
  pos2 = 4,
  vals1 = c(200, 500, 1000, 2000, 3000),
  vals2 = c(100, 200, 300, 400, 500),
  lab1 = "K[F]",
  lab2 = "K[UF]",
  x = seq(0, 10, 0.1),
  model = model(), 
  pars = pars
)

# Save
ggsave("plots/pips_carrying_capacity_facilitated_vs_unfacilitated.png", plot, width = 8, height = 7, dpi = 300)
