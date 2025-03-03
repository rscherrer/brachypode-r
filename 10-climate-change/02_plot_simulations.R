# Here we plot the results of our climate change experiment --- different
# simulations according to different climate change scenarios.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(brachypoder)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Load an example simulation
simfile <- "data/stress-increase-K2-100/sim-1"

# Plot individual trait values through time
plot1 <- read_individual_data(simfile) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = x, color = factor(patch))) +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray90")) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Plot census data through time
plot2 <- read_patch_size_data(simfile) %>%
  mutate(patch = if_else(patch == 0, "UF", "F")) %>%
  ggplot(aes(x = time, y = n, color = factor(patch))) +
  geom_line() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Read parameters of the simulation
pars <- read_parameters(simfile)

# Compute rates of change of key parameters under climate change
rate_theta <- with(pars, (stress[1] - stress[2]) / twarming)
rate_K <- with(pars, (capacities[1] - capacities[2]) / twarming)

# Make a table with changing parameter values
data <- tibble(
  time = seq(0, pars$tend, 1000),
  theta1 = if_else(time < pars$tchange, pars$stress[2], pars$stress[2] + rate_theta * (time - pars$tchange)),
  theta2 = pars$stress[1],
  K1 = if_else(time < pars$tchange, pars$capacities[2], pars$capacities[2] + rate_K * (time - pars$tchange)),
  K2 = pars$capacities[1]
)

# Plot the change in environmental stress through time
plot3 <- data %>%
  pivot_longer(theta1:theta2) %>%
  mutate(name = if_else(name == "theta1", "F", "UF")) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab(parse(text = "'Stress level ('*theta*')'")) +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4)

# Plot the change in carrying capacity
plot4 <- data %>%
  pivot_longer(K1:K2) %>%
  mutate(name = if_else(name == "K1", "F", "UF")) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("gray20", "gray80")) +
  xlab("Time (generations)") +
  ylab("Carrying capacity (K)") +
  labs(color = "Patch") +
  geom_vline(xintercept = 10000, linetype = 4) +
  ylim(c(0, max(pars$capacities)))

# Combine the plots
P0 <- wrap_plots(
  plot1 + theme(legend.position = "none") + rm_axis("x"), 
  plot2 + theme(legend.position = "none") + rm_axis("x"), 
  plot3, 
  plot4, 
  ncol = 2, guides = "collect", heights = c(6, 5)
) + plot_annotation(tag_levels = "A")

# Save
ggsave("plots/climate_change_example.png", P0, width = 6, height = 4, dpi = 300)

# List of sets of simulations (one per climate change scenario)
sets <- list.dirs("data", recursive = FALSE)

# For each set...
plots <- map(sets, function(set) {
  
  # Display
  print(set)
  
  # For each simulation within that set...
  data <- map_dfr(list.dirs(set)[-1], function(dir) {
    
    # Read the parameters
    pars <- read_parameters(dir)
    
    # Read the simulation data and append key parameters
    read_patch_size_data(dir) %>%
      mutate(
        twarming = pars$twarming,
        pgoodEnd = pars$pgoodEnd,
        capacitiesEnd2 = pars$capacitiesEnd[2],
        stressEnd2 = pars$stressEnd[2],
        capacities1 = pars$capacities[1]
      )
    
  }, .id = "sim")
  
  # Add labels
  data <- data %>% add_labels("twarming", "Delta*t[W]")
  
  # Figure how long each population survived
  tdata <- data %>%
    group_by(twarming_lab) %>%
    summarize(tend = max(time))
  
  # Plot the numbers of individuals through time
  plot <- data %>%
    mutate(patch = if_else(patch == 0, "UF", "F")) %>%
    ggplot(aes(x = time / 1000, y = n, color = factor(patch))) +
    geom_line() +
    facet_wrap(. ~ twarming_lab, labeller = label_parsed) +
    scale_color_manual(values = c("gray20", "gray80")) +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ylab("No. individuals") +
    labs(color = "Patch") +
    geom_vline(xintercept = 10, linetype = 4) +
    geom_rect(aes(xmin = tend / 1000, color = NULL, x = NULL, y = NULL), data = tdata, xmax = 20, ymin = 0, ymax = max(data$n), fill = "gray20", alpha = 0.5)
  
  # Prepare figure name
  figname <- paste0("plots/extinction_", str_remove(set, "data/"), ".png")
  figname <- str_replace_all(figname, "-", "_")
  
  # Save separate plots
  ggsave(figname, plot, width = 6, height = 3, dpi = 300)
  
  # But keep the plot
  return(plot)
  
})

# Update names
names(plots) <- str_remove(sets, "data/")

# Reorder
plots <- plots[c(
  "stress-increase-K2-100", "landscape-deterioration", 
  "cover-shrinkage-K2-100", "macro-mutations"
)]

# Update facets and labels
plots <- map(plots, ~ .x + facet_grid(. ~ twarming_lab, labeller = label_parsed))

# Add titles
titles <- c("Stress increase", "Landscape deterioration", "Cover shrinkage", "Cover shrinkage with macro-mutations")
plots <- map2(plots, titles, ~ .x + ggtitle(.y))

# Customization
plots[2:4] <- map(plots[2:4], ~ .x + rm_strips("x"))
plots[1:3] <- map(plots[1:3], ~ .x + rm_axis("x"))
plots <- map(plots, ~ .x + theme(legend.position = "none"))
plots <- map(plots, ~ .x + scale_x_continuous(breaks = c(0, 10, 20)))

# Combine with the previous patchwork
P <- wrap_plots(P0, wrap_plots(plots, ncol = 1), ncol = 1, heights = c(3, 5)) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/climate_change.png", P, width = 8, height = 10, dpi = 300)
