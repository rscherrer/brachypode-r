## Here we plot the numbers of individuals through time across
## simulations.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# Make plots
plot <- PLOTFUN("../data/climate-change/standard/", tmax = 40000)
plot_long <- PLOTFUN("../data/climate-change/long/", tmax = 80000, show_titles = FALSE, show_y = FALSE, show_xlab = FALSE, rm_legend = FALSE)

# Combine
plot <- wrap_plots(plot, plot_long, nrow = 1, widths = c(4, 1), guides = "collect")

# Save
ggsave("plots/climate_change_densities.png", plot, width = 9, height = 6, dpi = 300)
