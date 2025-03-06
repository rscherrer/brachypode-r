# Here we plot the traits of individuals through time across
# simulations.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(brachypoder)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)
for (f in list.files("functions", full.names = TRUE)) source(f)

# Make plots
plot <- PLOTFUN("data/standard/", tmax = 40000, plot_traits = TRUE, ymax = 20)
plot_long <- PLOTFUN("data/long/", tmax = 80000, plot_traits = TRUE, show_titles = FALSE, show_y = FALSE, show_xlab = FALSE, ymax = 20)

# Combine
plot <- wrap_plots(plot, plot_long, nrow = 1, widths = c(4, 1))

# Save
ggsave("plots/climate_change_traits.png", plot, width = 8, height = 8, dpi = 300)
