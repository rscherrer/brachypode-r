## Here we plot the result of the climate change experiment across different
## numbers of demes.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# Paths to the data
paths <- c(
  "../data/climate-change",
  "../data/climate-change-outcrossing",
  "../data/climate-change-metapop/"
)

# Simulation directories
dirs <- reduce(map(paths, list.dirs), c)
dirs <- dirs[str_detect(dirs, "/sim-")]

# Possible scenarios
scenarios <- c(
  "Stress increase under the shrubs",
  "Whole landscape deterioration",
  "Shrub cover shrinkage"
)

# For each simulation...
data <- map_dfr(dirs, function(dir) {

  # Read data
  data <- readsim::read_data(dir, c("time", "popsize"))

  # Read parameters
  pars <- read_parameters(dir)

  # Extract the relevant ones
  twarming <- pars$twarming
  tend <- pars$tend
  selfing <- pars$selfing
  ndemes <- pars$ndemes

  # Convert selfing to outcrossing
  outcrossing <- 1 - selfing

  # Identify scenario from name
  scenario <- scenarios[1]
  if (str_detect(dir, "landscape-deterioration")) scenario <- scenarios[2]
  if (str_detect(dir, "cover-shrinkage")) scenario <- scenarios[3]

  # Reduce
  data <- data[nrow(data),]

  # Combine
  tibble(data, twarming, tend, outcrossing, ndemes, scenario)

}, .id = "sim")

# Reorder
data$scenario <- factor(data$scenario, levels = scenarios)

# Facet labels
data <- data %>%
  add_labels("outcrossing", "g") %>%
  mutate(scenario = str_c("'", scenario, "'"))

# Plot
plot <- data %>%
  ggplot(aes(x = factor(ndemes), y = factor(twarming))) +
  facet_grid(scenario ~ outcrossing_lab, labeller = label_parsed) +
  geom_tile(aes(fill = time / tend)) +
  scale_fill_gradient(low = "white", high = "black", limits = c(0, NA)) +
  ylab(parse(text = "'Warming time ('*Delta*t[W]*')'")) +
  xlab(parse(text = "'Number of demes ('*n[D]*')'")) +
  labs(fill = "End time (%)")

# Save
ggsave("plots/climate_change_metapop.png", plot, width = 8, height = 7, dpi = 300)
