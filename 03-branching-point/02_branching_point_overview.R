## Here we produce a figure that summarizes some of the parameters that 
## determine evolutionary branching, and illustrate what happens then with
## some simulations.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(rlang)

theme_set(theme_classic())

source("../functions.R")

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 500,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.3
  
)

# Create a series of PIPs across various carrying capacities
p1 <- plot_pip_transect(
  par = "K2", 
  pos = 4, 
  vals = c(500, 300, 100, 50), 
  lab = "K[UF]", 
  x = seq(0, 10, 0.1),
  model = model(),
  pars, 
  extra = FALSE
)

# For each simulation...
data <- map_dfr(list.dirs("../data/branching-point/overview")[-1], function(dir) {
  
  # Read the parameters
  pars <- read_parameters(dir)
  
  # Read the simulation data
  read_individual_data(dir) %>%
    mutate(allfreq = pars$allfreq)
  
}, .id = "sim")

# Plot individual phenotypes through time
p2 <- data %>%
  filter(time %% 200 == 0) %>%
  ggplot(aes(x = time, y = x, color = allfreq)) +
  geom_point() +
  scale_color_gradient(low = "gray80", high = "gray20", guide = "legend", breaks = seq(0, 1, 0.2)) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = parse(text = "p[0]"))

# Read census data in one simulation in particular (one with branching)
data2 <- read_patch_size_data("../data/branching-point/overview/sim-6")

# Plot the numbers of individuals in each patch through time
p3 <- data2 %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = n, linetype = patch)) +
  geom_line(color = "gray20") +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(linetype = "Patch") +
  guides(color = "none")

# Combine all plots
P <- wrap_plots(p1, wrap_plots(p2, p3, nrow = 1), ncol = 1) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/branching_point_overview.png", P, width = 8, height = 5, dpi = 300)
