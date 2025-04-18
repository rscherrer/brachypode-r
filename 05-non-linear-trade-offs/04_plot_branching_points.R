## Here we explore some branching points in particular.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

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

# Plot a series of PIPs across trade-off curves
plot1 <- plot_pip_transect(
  par = "epsilon", 
  pos = 2, 
  vals = seq(0.1, 0.5, 0.1), 
  lab = "epsilon", 
  x = seq(0, 10, 0.1),
  model = model2(), 
  pars = pars, 
  tend = 1000,
  extra = TRUE
)

# Same but for a series of MIPs
plot2 <- plot_mip_transect(
  par = "epsilon", 
  pos = 2, 
  vals = seq(0.1, 0.5, 0.1), 
  lab = "epsilon", 
  x = seq(0, 10, 0.1), 
  model = model2(), 
  pars = pars, 
  tend = 1000,
  field = seq(0, 10, 0.25),
  scale = 0.2,
  model_di = model_di2()
)

# For each simulation...
simdata <- map_dfr(list.dirs("../data")[-1], function(dir) {
  
  # Read trait values
  read_individual_data(dir) %>%
    group_by(time) %>%
    summarize(
      x1 = mean(x[x < mean(x)]), 
      x2 = mean(x[x > mean(x)])
    ) %>%
    ungroup() %>%
    mutate(epsilon = read_parameters(dir)$tradeoff)
  
}, .id = "sim")

# Rename things
simdata <- simdata %>% 
  rename(value = "epsilon") %>%
  add_labels("value", "epsilon")

# Overlay trait values on top of the MIPs
plot2 <- plot2 +
  geom_path(data = simdata, aes(fill = NULL), color = "black") +
  geom_point(data = simdata, aes(fill = NULL), color = "black", size = 2)

# Combine the plots
P <- wrap_plots(
  plot1, 
  plot2 + rm_strips("x"), 
  ncol = 1, heights = c(6, 3)
) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/branching_point_non_linear_trade_offs.png", P, width = 8.5, height = 6, dpi = 300)
