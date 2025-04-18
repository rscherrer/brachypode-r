## Here we plot the results of the coexistence analysis across parameter space.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

# Load the data
data <- readRDS("data/parspace.rds")

# For each summary statistic...
for (var in c("area", "maxdiff", "meandiff", "vardiff", "propdiff", "convarea", "ddimcoal")) {
  
  # Display
  print(var)
  
  # Plot the statistic across parameter space
  p <- data %>%
    ggplot(aes(x = factor(theta2), y = factor(epsilon), fill = get(var))) +
    geom_tile() +
    facet_grid(K2_lab ~ c_lab, labeller = label_parsed) +
    xlab(parse(text = "'Stress in unfacilitated patches ('*theta[UF]*')'")) +
    ylab(parse(text = "'Trade-off ('*epsilon*')'")) +
    labs(fill = var)
  
  # Save
  ggsave(paste0("plots/mutual_invasibility_", var, ".png"), p, width = 5, height = 3, dpi = 300)
  
}

# Final summary plot
plot <- data %>%
  ggplot(aes(x = factor(theta2), y = factor(epsilon), fill = area)) +
  geom_tile() +
  facet_grid(K2_lab ~ c_lab, labeller = label_parsed) +
  xlab(parse(text = "'Stress in unfacilitated patches ('*theta[UF]*')'")) +
  ylab(parse(text = "'Trade-off ('*epsilon*')'")) +
  scale_fill_gradient(low = "black", high = "white") +
  guides(fill = guide_colorbar(barheight = 5, title = "AMI", order = 1)) +
  geom_tile(aes(color = BP), size = 1, data = data %>% filter(BP == "BP")) +
  scale_color_manual(values = "lightsalmon") +
  labs(color = NULL) +
  ggnewscale::new_scale_color() +
  geom_point(data = data %>% filter(nsims > 0), aes(color = propdiff), size = 2) +
  geom_point(data = data %>% filter(nsims > 0), aes(fill = NULL), shape = 21, color = "black", size = 2) +
  scale_color_gradient(low = "gray30", high = "yellow", guide = "legend") +
  guides(color = guide_legend(keyheight = 0.8, title = "% dim.", order = 2))

# Save
ggsave("plots/mutual_invasibility_2.png", plot, width = 6, height = 3.5, dpi = 300)

# Load the simulation line plots
lines <- readRDS("data/lines.rds")

# Load the MIP
mip <- readRDS("data/mip_simulations.rds")

# Combine plots
P <- wrap_plots(
  wrap_plots(
    mip + theme(legend.position = "none"), 
    plot, 
    nrow = 1, widths = c(2, 3)
  ),
  lines + facet_grid(. ~ sim_lab, labeller = label_parsed), 
  ncol = 1, guides = "collect", heights = c(2, 1)
) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/mutual_invasibility_3.png", P, width = 9, height = 6, dpi = 300)

# New plot
plot2 <- data %>%
  ggplot(aes(x = factor(theta2), y = factor(epsilon), fill = area)) +
  geom_tile() +
  facet_grid(K2_lab ~ c_lab, labeller = label_parsed) +
  xlab(parse(text = "'Stress in unfacilitated patches ('*theta[UF]*')'")) +
  ylab(parse(text = "'Trade-off ('*epsilon*')'")) +
  scale_fill_gradient(low = "black", high = "white") +
  guides(fill = guide_colorbar(barheight = 3, title = "AMI", order = 1)) +
  geom_tile(aes(color = BP), size = 1, data = data %>% filter(BP == "BP")) +
  scale_color_manual(values = "lightsalmon") +
  labs(color = NULL) +
  ggnewscale::new_scale_color() +
  geom_point(data = data %>% filter(!is.na(convarea)), aes(color = convarea, size = ddimcoal)) +
  geom_point(data = data %>% filter(!is.na(convarea)), aes(fill = NULL, size = ddimcoal), shape = 21, color = "black") +
  scale_color_gradient(low = "gray30", high = "yellow", limits = c(0, 1)) +
  scale_size_continuous(breaks = c(1, 3, 5)) +
  guides(
    color = guide_colorbar(barheight = 3, title = "BA", order = 2),
    size = guide_legend(keyheight = 0.4, title = parse(text = "Delta*x"), order = 4)
  ) +
  theme(legend.spacing = unit(0.001, "in"))

# Save
ggsave("plots/mutual_invasibility.png", plot2, width = 6, height = 3.5, dpi = 300)
