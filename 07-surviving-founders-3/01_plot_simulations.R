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

# Parameter values
pars <- alist(

  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 500,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.1

)

# Generate a PIP
pip <- plot_pip(seq(0, 10, 0.1), model(), pars)

# Plot how long it takes before the population goes extinct
lolliplot <- data %>%
  group_by(sim) %>%
  filter(time == max(time)) %>%
  ggplot(aes(x = factor(allfreq), y = time, fill = factor(outcrossing))) +
  geom_bar(stat = "identity", position = "dodge2") +
  xlab(parse(text = "'Initial allele frequency ('*p[0]*')'")) +
  ylab("End time (generations)") +
  labs(fill = "g") +
  scale_fill_manual(values = c("gray20", "gray35", "gray50", "gray65", "gray80"))

# Combine
P <- wrap_plots(pip, lolliplot) + plot_annotation(tag_levels = "A")

# Save
ggsave("plots/surviving_founders_lolliplot.png", P, width = 8, height = 3, dpi = 300)
