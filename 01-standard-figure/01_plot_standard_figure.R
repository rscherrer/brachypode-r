## Here we generate a figure showing the general behavior and features of our
## model in some standard combination of parameters, just to illustrate.

rm(list = ls())

library(tidyverse)
library(patchwork)

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

# Generate a PIP
p1 <- plot_pip(seq(0, 10, 0.1), model(), pars)

# For each simulation...
data <- map_dfr(list.dirs("../data/standard-figure/")[-1], function(dir) {
  
  # Read the parameters
  pars <- read_parameters(dir)
  
  # Read the simulation data
  read_individual_data(dir) %>%
    mutate(allfreq = pars$allfreq)
  
}, .id = "sim")

# Plot individual traits through time
p2 <- data %>%
  filter(time %% 200 == 0) %>%
  ggplot(aes(x = time, y = x, color = allfreq)) +
  geom_point() +
  scale_color_gradient(low = "gray80", high = "gray20", guide = "legend", breaks = seq(0, 1, 0.2)) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = parse(text = "p[0]"))

# For each simulation...
data2 <- map_dfr(list.dirs("../data/standard-figure/")[-1], function(dir) {
  
  # Read the parameters
  pars <- read_parameters(dir)
  
  # Read the census data
  read_patch_size_data(dir) %>%
    mutate(allfreq = pars$allfreq)
  
}, .id = "sim")

# Plot abundance in each patch through time
p3 <- data2 %>%
  add_labels("allfreq", "p[0]") %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time / 1000, y = n, color = allfreq, linetype = patch)) +
  geom_line() +
  facet_grid(. ~ allfreq_lab, labeller = label_parsed) +
  scale_color_gradient(low = "gray80", high = "gray20") +
  xlab(parse(text = "'Time ('*10^3~'generations)'")) +
  ylab("No. individuals") +
  labs(linetype = "Patch") +
  guides(color = "none")

# Combine plots
P <- wrap_plots(
  wrap_plots(p1, p2, nrow = 1, widths = c(2, 3)),
  p3,
  ncol = 1,
  heights = c(3, 2)
) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/standard_figure.png", P, width = 10, height = 6, dpi = 300)
