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
plot_long <- PLOTFUN("data/long/", tmax = 80000, plot_traits = TRUE, show_titles = FALSE, show_y = FALSE, show_xlab = FALSE, ymax = 20, rm_legend = FALSE)

# Resize plots
plot <- plot + plot_layout(heights = c(1, 1, 1, 2))
plot_long <- plot_long + plot_layout(heights = c(1, 1, 1, 2))

# Resize some y-axes
for (i in 1:3) plot[[i]] <- plot[[i]] + ylim(c(0, 10))
for (i in 1:3) plot_long[[i]] <- plot_long[[i]] + ylim(c(0, 10))

# Remove some labels
for (i in c(1, 3)) plot[[i]] <- plot[[i]] + ylab(NULL)

# Combine
plot <- wrap_plots(plot, plot_long, nrow = 1, widths = c(4, 1), guides = "collect")

# Save
ggsave("plots/climate_change_traits.png", plot, width = 10, height = 8, dpi = 300)
