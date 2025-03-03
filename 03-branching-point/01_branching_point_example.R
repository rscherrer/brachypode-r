# Here we produce an overview figure to illustrate what a branching point is.

rm(list = ls())

library(tidyverse)
library(brachypoder)
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

# Plot a PIP showing the branching point
p <- plot_pip(seq(0, 10, 0.1), model(), pars)

# Make the corresponding MIP (takes a while)
p2 <- plot_mip(

  seq(0, 10, 0.1),
  model(),
  pars,
  field = seq(0, 10, 0.25),
  scale = 0.2,
  model_di = model_di(),
  grid = seq(0, 10, 0.2)

)

# Run a deterministic simulation (which should branch hopefully)
simdata <- simulate(model(), pars, x = 7, ntimes = 1000, model_di = model_di(), passon = TRUE, sigma = 0.5)

# Plot the simulation
p3 <- simdata %>%
  ggplot(aes(x = time, y = x1)) +
  geom_line() +
  geom_line(aes(y = x2)) +
  xlab("Time (generations)") +
  ylab(parse(text = "'Stress tolerance ('*hat(x)*')'"))

# Plot an individual-based stochastic simulation that also branches
p4 <- read_individual_data("data/example/sim-1") %>%
  filter(time %% 1000 == 0) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = x, color = patch)) +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = NULL)

# Store the data from that simulation
stochdata <- read_individual_data("data/example/sim-1") %>%
  group_by(time) %>%
  summarize(x1 = mean(x[x < mean(x)]), x2 = mean(x[x > mean(x)]))

# Overlay them on top of the MIP
p2 <- p2 +
  geom_path(data = simdata, aes(fill = NULL), color = "white") +
  geom_point(data = simdata, aes(fill = NULL), color = "white") +
  geom_path(data = stochdata, aes(fill = NULL), color = "black") +
  geom_point(data = stochdata, aes(fill = NULL), color = "black", size = 2)

# Combine everything
P <- wrap_plots(p, p2, p3, p4, ncol = 2, nrow = 2) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/branching_point_example.png", P, width = 8, height = 6, dpi = 300)
