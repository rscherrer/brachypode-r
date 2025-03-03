# Here we plot some PIPs across some parameter values.

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
  K2 <- 2000,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5

)

# Plot the PIPs
plot <- plot_pip_grid(
  par1 = "K2",
  par2 = "c",
  pos1 = 4,
  pos2 = 8,
  vals1 = c(2000, 1000, 100, 500, 100),
  vals2 = c(0.1, 0.3, 0.5, 0.7, 0.9),
  lab1 = "K[UF]",
  lab2 = "c",
  model = model(),
  pars = pars,
  x = seq(0, 10, 0.1)
)

# Save
ggsave("plots/pips_carrying_capacity_by_shrub_cover.png", plot, width = 8, height = 6, dpi = 300)
