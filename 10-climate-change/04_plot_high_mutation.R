# Here we plot the traits of individuals through time across
# a specific set of simulations with varying mutation rates.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(brachypoder)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)
source("functions/get_scenario_names.R")

# Plotting function
PLOTFUN <- function(set, tmax = 40000, ymax = 10) {
  
  # set: the name of set of simulations to plot
  # tmax: maximum simulation time
  # ymax: limit of the y-axis
  
  # Locate simulation directories
  dirs <- str_c("data/standard/", set, "/sim-5")
  dirs <- c(dirs, list.dirs(str_c("data/highmut/", set), full.names = TRUE)[-1])
  
  # For each directory...
  data <- map_dfr(dirs, function(dir) {
    
    # Read the data
    data <- read_individual_data(dir)
    
    # Read the parameters
    pars <- read_parameters(dir)
    
    # Append key parameters
    data %>%
      mutate(
        twarming = pars$twarming,
        pgoodEnd = pars$pgoodEnd,
        capacitiesEnd2 = pars$capacitiesEnd[2],
        stressEnd2 = pars$stressEnd[2],
        capacities1 = pars$capacities[1],
        mutation = pars$mutation
      )
    
  }, .id = "sim")
  
  # Add labels
  data <- data %>% add_labels("mutation", "mu")
  
  # Figure how long each population survived
  tdata <- data %>%
    group_by(mutation_lab) %>%
    summarize(tend = max(time))
  
  # Set the plot up
  data %>%
    mutate(patch = if_else(patch == 0, "UF", "F")) %>%
    ggplot(aes(x = time / 1000, y = x, color = factor(patch))) +
    geom_point() +
    ylab("Stress tolerance (x)") +
    facet_grid(. ~ mutation_lab, labeller = label_parsed) +
    scale_color_manual(values = c("gray20", "gray80")) +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    labs(color = "Patch") +
    xlim(c(0, tmax / 1000)) +
    ylim(c(0, ymax)) +
    geom_vline(xintercept = 10, linetype = 4) +
    geom_rect(aes(xmin = tend / 1000, color = NULL, x = NULL, y = NULL), data = tdata, xmax = tmax / 1000, ymin = 0, ymax = ymax, fill = "gray20", alpha = 0.5, show.legend = FALSE)
  
}

# Plot
p1 <- PLOTFUN("landscape-deterioration") + ggtitle(get_scenario_names()[2])
p2 <- PLOTFUN("cover-shrinkage-K2-100") + ggtitle(get_scenario_names()[3])

# Tweak
p1 <- p1 + rm_axis("x")
p2 <- p2 + rm_strips("x")

# Combine
plot <- wrap_plots(p1, p2, ncol = 1, guides = "collect")

# Save
ggsave("plots/climate_change_highmut.png", plot, width = 9, height = 4, dpi = 300)
