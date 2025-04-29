## Here we plot a heatmap describing the minimum required mutation rate needed
## to rescue a population under various rates of environmental deterioration
## (specifically of the UF patches - the F patches deteriorate at the same
## rate)

rm(list = ls())

library(tidyverse)
library(readsim)

source("../functions.R")

theme_set(theme_classic())

# Path to the data
dir <- "../data/mutational-rescue/data"

# List of simulation folders
folders <- find_simulation_folders(dir, pattern = "sim_")

# For each folder...
data <- map_dfr(folders, function(folder) {
  
  # Read the recorded time points
  times <- read_data(folder, variables = "time")[[1]]
  
  # Maximum time reached 
  tmax <- last(times)
  
  # Read parameters
  pars <- read_parameters(folder)
  
  # Return a table
  tibble(
    stressEnd0 = pars[["stressEnd"]][1],
    capacitiesEnd0 = pars[["capacitiesEnd"]][1],
    mutation = pars[["mutation"]],
    tend = pars[["tend"]],
    tmax = tmax
  )
  
}, .id = "sim")

# Frequency of rescue
data <- data %>%
  group_by(stressEnd0, capacitiesEnd0, mutation) %>%
  summarize(freq = sum(tmax == tend)) %>%
  ungroup()

# Function to add an apostrophe to a plotmath label
prime <- function(x) str_c(x, "*", '"', "'", '"')

# Make labels
data <- data %>%
  add_labels("stressEnd0", str_c(prime("theta"), "[UF]")) %>%
  add_labels("capacitiesEnd0", str_c(prime("K"), "[UF]"))

# Combine labels
data <- data %>%
  mutate(
    pace = str_c("atop(", stressEnd0_lab, ", ", capacitiesEnd0_lab, ")"),
    pace = fct_reorder(pace, stressEnd0)
  )

# Plot
plot <- data %>%
  ggplot(aes(x = pace, y = factor(mutation), fill = 10 * freq)) +
  geom_tile() +
  ylab(parse(text = "'Mutation rate ('*mu*')'")) +
  xlab("Deterioration of the UF patch") +
  labs(fill = "% surv.") +
  scale_x_discrete(labels = function(x) parse(text = x)) +
  scale_fill_gradient(low = "black", high = "mediumpurple1")

# Save
ggsave("plots/mutational_rescue.png", plot, width = 5, height = 4, dpi = 300)
