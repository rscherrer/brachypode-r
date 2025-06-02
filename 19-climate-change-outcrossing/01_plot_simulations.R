## Here we plot the result of the climate change experiment across different 
## levels of outcrossing

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# Path to the data
path <- "../data/climate-change-outcrossing/"

# Simulation directories
dirs <- list.dirs(path)
dirs <- dirs[str_detect(dirs, "/sim-")]

# Old simulations (no outcrossing)
oldirs <- list.dirs("../data/climate-change")
oldirs <- oldirs[str_detect(oldirs, "/sim-")]
oldirs <- oldirs[!str_detect(oldirs, "highmut")]
oldirs <- oldirs[map_lgl(oldirs, ~ any(str_detect(.x, c("stress-increase-K2-100", "cover-shrinkage-K2-100", "landscape-deterioration"))))]

# Append
dirs <- c(dirs, oldirs)

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
  
  # Convert selfing to outcrossing
  outcrossing <- 1 - selfing
  
  # Identify scenario from name
  scenario <- scenarios[1]
  if (str_detect(dir, "landscape-deterioration")) scenario <- scenarios[2]
  if (str_detect(dir, "cover-shrinkage")) scenario <- scenarios[3]
  
  # Append
  tibble(data, twarming, tend, outcrossing, scenario)
  
}, .id = "sim")

# Reorder
data$scenario <- factor(data$scenario, levels = scenarios)

# Facet labels
data <- data %>% 
  add_labels("twarming", "Delta*t[W]") %>%
  mutate(
    scenario_lab = str_c("'", scenario, "'"),
    scenario_lab = fct_reorder(scenario_lab, labels(scenario))
  )

# Plotting function
PLOTFUN <- function(data, maxpop = NA) {
  
  # data: the data
  # maxpop: maximum population size
  
  # Plot
  data %>%
    ggplot(aes(x = time, y = factor(outcrossing))) +
    facet_grid(twarming_lab ~ scenario_lab, labeller = label_parsed) +
    geom_tile(aes(fill = popsize)) +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, maxpop)) +
    xlab("Time (generations)") +
    ylab("Outcrossing (g)") +
    labs(fill = "Pop. size") +
    geom_vline(xintercept = 10000, linetype = 4)
  
}

# Maximum population size
maxpop <- max(data$popsize)

# Make plots
p1 <- data %>% filter(tend < 80000) %>% PLOTFUN(maxpop)
p2 <- data %>% filter(tend == 80000) %>% PLOTFUN(maxpop)

# Customize
p1 <- p1 + xlab(NULL)
p2 <- p2 + rm_strips("x") + theme(legend.position = "none") + ylab(NULL)

# Combine
P <- wrap_plots(p1, p2, ncol = 1, heights = c(6, 1))

# Save
ggsave("plots/climate_change_outcrossing.png", P, width = 8, height = 7, dpi = 300)
