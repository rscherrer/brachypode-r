# Here we load the MIP and overlay the simulations on top of it.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

# Load functions
for (f in list.files("../functions", full.names = TRUE)) source(f)

# Read the simulated data
simdata <- readRDS("data/simdata.rds")

# Wrangle
simdata <- simdata %>%
  select(x1, x2, data) %>%
  rename(x01 = "x1", x02 = "x2") %>%
  mutate(sim = seq(n())) %>%
  unnest(data)

# Add labels
simdata <- simdata %>%
  add_labels("sim", "'Sim.'", sep = " ")

# Plot the simulations
lines <- simdata %>%
  ggplot(aes(x = time, y = x1, color = factor(sim))) +
  geom_line() +
  geom_line(aes(y = x2)) +
  facet_wrap(. ~ sim_lab, labeller = label_parsed) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  guides(color = "none") +
  geom_point(data = simdata %>% filter(time == min(time)), shape = 21, fill = "white") +
  geom_point(aes(y = x2), data = simdata %>% filter(time == min(time)), shape = 21, fill = "white")

# Save the plot for later
saveRDS(lines, "data/lines.rds")

# Starting points
smrdata <- simdata %>%
  group_by(sim) %>%
  filter(time == min(time))

# Load the MIP
mip <- readRDS("data/mip.rds")

# Add simulations to the MIP
mip <- mip +
  geom_point(data = simdata, aes(color = factor(sim), fill = NULL), size = 1) +
  geom_point(data = smrdata, aes(color = factor(sim)), shape = 21, fill = "white", size = 2) +
  guides(color = "none") +
  scale_size(range = c(1, 0.2)) +
  labs(alpha = "Time (gens)")

# Save the MIP overlay
saveRDS(mip, "data/mip_simulations.rds")

# Combine the plots
P <- wrap_plots(lines, mip, nrow = 1, widths = c(4, 3)) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/mip_simulations.png", P, width = 9, height = 4, dpi = 300)
