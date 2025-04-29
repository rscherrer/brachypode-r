## Here we plot some stochastic simulations.

rm(list = ls())

library(tidyverse)

theme_set(theme_classic())

source("../functions.R")

# Read mean traits through time
dirs <- list.dirs("../data/simplest-case/")[-1]
data <- map_dfr(dirs, read_trait_mean_data, .id = "sim")

# Plot the simulations
plot <- data %>%
  mutate(
    patch = fct_recode(factor(patch), "F" = "1", "UF" = "0"),
    sim = str_c("Sim. ", sim)
  ) %>%
  ggplot(aes(x = time, y = x, linetype = factor(patch), color = sim)) +
  geom_line() +
  ylab("Stress tolerance") +
  xlab("Time") +
  labs(linetype = NULL, color = NULL)

# Save
ggsave("plots/simplest_case.png", plot, width = 4, height = 3, dpi = 300)
