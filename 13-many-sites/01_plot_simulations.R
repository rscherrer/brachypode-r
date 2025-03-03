# Here we plot what goes on in metapopulations with many sites.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(brachypoder)
library(ggbeeswarm)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# For each simulation...
data <- map_dfr(list.dirs("data")[-1], function(dir) {

  # Read parameter values
  pars <- read_parameters(dir)

  # Read individual trait values
  read_individual_data(dir) %>%
    mutate(
      ndemes = pars$pgood[1],
      allfreq = pars$allfreq,
      capacityUF = pars$capacities[1]
    )

}, .id = "sim")

# For each level of carrying capacity...
data <- data %>%
  group_by(capacityUF) %>%
  nest() %>%
  mutate(plot = map2(data, capacityUF, function(data, capacityUF) {

    # Plot the simulations
    data %>%
      filter(time %% 1000 == 0) %>%
      add_labels("ndemes", "n[D]") %>%
      add_labels("allfreq", "p[0]") %>%
      ggplot(aes(x = time, y = x, color = factor(deme))) +
      geom_point() +
      facet_grid(ndemes_lab ~ allfreq_lab, labeller = label_parsed) +
      xlab("Time (generations)") +
      ylab("Stress tolerance (x)") +
      labs(color = "Deme") +
      ggtitle(parse(text = paste0("K[UF]~'= ", capacityUF, "'"))) +
      rm_strips("y")

  }))

# Customization
data$plot[[1]] <- data$plot[[1]] + rm_axis("x")
data$plot[[2]] <- data$plot[[2]] + rm_strips("x")

# Combine the plots
P1 <- wrap_plots(data$plot, ncol = 1)

# Now, again for each simulation...
data2 <- map_dfr(list.dirs("data")[-1], function(dir) {

  # Read the parameter values
  pars <- read_parameters(dir)

  # Read census data
  read_patch_size_data(dir) %>%
    mutate(
      ndemes = pars$pgood[1],
      allfreq = pars$allfreq,
      capacityUF = pars$capacities[1]
    )

}, .id = "sim")

# For each level of carrying capacity...
data2 <- data2 %>%
  group_by(capacityUF) %>%
  nest() %>%
  mutate(plot = map(data, function(data) {

    # Plot the simulations
    data %>%
      add_labels("ndemes", "n[D]") %>%
      add_labels("allfreq", "p[0]") %>%
      ggplot(aes(
        x = time, y = n, color = factor(deme), linetype = factor(patch),
        group = interaction(patch, deme)
      )) +
      geom_line() +
      facet_grid(ndemes_lab ~ allfreq_lab, labeller = label_parsed) +
      xlab("Time (generations)") +
      ylab("No. individuals") +
      labs(color = "Deme", linetype = "Patch") +
      guides(color = "none") +
      rm_strips("y")

  }))

# Customization
data2$plot[[1]] <- data2$plot[[1]] + rm_axis("x")
data2$plot[[2]] <- data2$plot[[2]] + rm_strips("x")

# Combine
P2 <- wrap_plots(data2$plot, ncol = 1)

# Then again for each set of simulations...
data <- data %>%
  mutate(plot2 = map(data, function(data) {

    # Plot traits at the end of the simulation as a bee swarm plot
    data %>%
      filter(time == max(time)) %>%
      add_labels("ndemes", "n[D]") %>%
      add_labels("allfreq", "p[0]") %>%
      ggplot(aes(x = factor(deme), y = x, color = factor(deme))) +
      geom_beeswarm(cex = 0.05) +
      facet_grid(ndemes_lab ~ allfreq_lab, labeller = label_parsed) +
      coord_flip() +
      scale_alpha_manual(values = c(0.1, 0.3)) +
      ylab("Stress tolerance (x)") +
      xlab("Deme") +
      guides(color = "none")

  }))

# Customization
data$plot2[[1]] <- data$plot2[[1]] + rm_axis("x")
data$plot2[[2]] <- data$plot2[[2]] + rm_strips("x")

# Combine
P3 <- wrap_plots(data$plot2, ncol = 1)

# Final patchwork
P <- wrap_plots(
  P1, P2, P3, nrow = 1, guides = "collect",
  widths = c(5, 5, 3)
) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("many_sites.png", P, width = 13, height = 8, dpi = 300)
