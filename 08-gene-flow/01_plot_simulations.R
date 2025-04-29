## Here we look at the effect of genee flow (in the form of outcrossing) on the
## divergence of morphs.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# For each simulation...
data <- map_dfr(list.dirs("../data/gene-flow")[-1], function(dir) {

  # Read the parameters
  pars <- read_parameters(dir)

  # Read trait values through time
  read_individual_data(dir) %>%
    mutate(gflow = 1 - pars$selfing)

}, .id = "sim")

# Plot trait values through time
plot <- data %>%
  add_labels("gflow", "g") %>%
  filter(time %% 500 == 0) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = x, color = patch)) +
  geom_point() +
  facet_grid(. ~ gflow_lab, labeller = label_parsed) +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = NULL)

# Save
ggsave("plots/gene_flow.png", plot, width = 8, height = 3, dpi = 300)
