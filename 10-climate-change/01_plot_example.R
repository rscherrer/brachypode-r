## Here we make a figure explaining our climate change experiment.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# Load an example simulation
dir <- "../data/standard/stress-increase-K2-100/sim-5"

# Plot individual trait values through time
plot1 <- read_individual_data(dir) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = x, color = factor(patch))) +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray90")) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Plot census data through time
plot2 <- read_patch_size_data(dir) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = n, color = factor(patch))) +
  geom_line() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Read parameters of the simulation
pars <- read_parameters(dir)

# Compute rates of change of key parameters under climate change
rate_theta <- with(pars, (stress[1] - stress[2]) / twarming)
rate_K <- with(pars, (capacities[1] - capacities[2]) / twarming)

# Make a table to track parameter change
data <- tibble(
  time = seq(0, pars$tend, 1000),
  theta1 = if_else(time < pars$tchange, pars$stress[2], pars$stress[2] + rate_theta * (time - pars$tchange)),
  theta2 = pars$stress[1],
  K1 = if_else(time < pars$tchange, pars$capacities[2], pars$capacities[2] + rate_K * (time - pars$tchange)),
  K2 = pars$capacities[1]
) %>% 
  mutate(
    theta1 = if_else(theta1 > theta2, theta2, theta1),
    K1 = if_else(K1 < K2, K2, K1)
  )

# Note: this way of making the table is very specific to the example scenario
# we have picked for the sake of the example.

# Plot the change in environmental stress through time
plot3 <- data %>%
  pivot_longer(theta1:theta2) %>%
  mutate(name = if_else(name == "theta1", "F", "UF")) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab(parse(text = "'Stress level ('*theta*')'")) +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Plot the change in carrying capacity
plot4 <- data %>%
  pivot_longer(K1:K2) %>%
  mutate(name = if_else(name == "K1", "F", "UF")) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("Carrying capacity (K)") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4) +
  ylim(c(0, max(pars$capacities)))

# Combine the plots
P0 <- wrap_plots(
  plot1 + theme(legend.position = "none") + rm_axis("x"), 
  plot2 + theme(legend.position = "none") + rm_axis("x"), 
  plot3, 
  plot4, 
  ncol = 2, guides = "collect", heights = c(6, 5)
) + plot_annotation(tag_levels = "A")

# Save
ggsave("plots/climate_change_example.png", P0, width = 7, height = 4, dpi = 300)
