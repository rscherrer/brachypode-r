## Here we look at the persistence time of various starting strategies
## in a stressful environmnent.

rm(list = ls())

library(tidyverse)
library(patchwork)

source("../functions.R")

theme_set(theme_classic())

# Simulation folders
paths <- list.dirs("../data/surviving-founders-3/", recursive = TRUE)
paths <- paths[str_detect(paths, "sim-")]

# For each simulation...
data <- map_dfr(
  paths, function(dir) {

    # Read the parameters
    pars <- read_parameters(dir)

    # Read the census data
    read_patch_size_data(dir) %>%
      mutate(allfreq = pars$allfreq, selfing = pars$selfing)

  }, .id = "sim"
)

# Function to format data
FORMAT <- function(data) {

  # Convert selfing to outcrossing
  data <- data %>% mutate(outcrossing = 1 - selfing)

  # Add labels
  data <- data %>%
    add_labels("allfreq", "p[0]") %>%
    add_labels("outcrossing", "g")

  return(data)

}

# Format
data <- data %>% FORMAT()

# Plot the number of individuals through time
plot <- data %>%
  filter(time %% 1000 == 0) %>%
  ggplot(aes(
    x = time, y = n, color = factor(deme), linetype = factor(patch),
    group = interaction(deme, patch)
  )) +
  geom_line() +
  facet_grid(outcrossing_lab ~ allfreq_lab, labeller = label_parsed) +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = "Deme", linetype = "Patch") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Save
ggsave("plots/surviving_founders_lines.png", plot, width = 12, height = 5, dpi = 300)

############################

# Plot how long it takes before the population goes extinct
lolliplot <- data %>%
  group_by(sim) %>%
  filter(time == max(time)) %>%
  ggplot(aes(x = allfreq, y = time)) +
  geom_segment(aes(xend = allfreq), yend = 0) +
  geom_point() +
  facet_wrap(. ~ outcrossing_lab, labeller = label_parsed) +
  xlab(parse(text = "'Initial allele frequency ('*p[0]*')'")) +
  ylab("End time (generations)")

# Save
ggsave("plots/surviving_founders_lolliplot.png", lolliplot, width = 6, height = 4, dpi = 300)
