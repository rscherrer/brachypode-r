## Here we look at the persistence time of various starting strategies
## in a stressful environmnent.

rm(list = ls())

library(tidyverse)
library(patchwork)

source("../functions.R")

theme_set(theme_classic())

# Paths to the simulations
paths <- c(
  "../data/surviving-founders-2/",
  "../data/surviving-founders-4/"
)

# Simulation folders
paths <- reduce(map(paths, list.dirs), c)
paths <- paths[str_detect(paths, "sim-")]

# For each simulation...
data <- map_dfr(
  paths, function(dir) {

    # Read the parameters
    pars <- read_parameters(dir)

    # Read the census data
    read_patch_size_data(dir) %>%
      mutate(
        allfreq = pars$allfreq,
        selfing = pars$selfing,
        ndemes = pars$ndemes
      )

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
data <- data %>% FORMAT() %>% add_labels("ndemes", "n[D]")

# Full data
data0 <- data

# Multiple sites only
data <- data %>% filter(ndemes > 1)

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

# Summarize across patches and demes
data_smr <- data0 %>%
  group_by(sim, time, allfreq, allfreq_lab, outcrossing, outcrossing_lab, ndemes, ndemes_lab) %>%
  summarize(n = sum(n))

# Plotting function
PLOTFUN <- function(data) {

  # Plot
  data %>%
    ggplot(aes(x = allfreq, y = time, color = n)) +
    geom_path(aes(group = allfreq)) +
    geom_point(
      data = data %>%
        group_by(sim) %>%
        filter(time == max(time))
    ) +
    facet_grid(ndemes_lab ~ outcrossing_lab, labeller = label_parsed) +
    xlab(parse(text = "'Initial allele frequency ('*p[0]*')'")) +
    ylab("End time (generations)") +
    labs(color = "Pop. size") +
    scale_color_gradient(low = "gray80", high = "black", limits = c(0, NA))

}

# Plot
lolliplot1 <- data_smr %>% filter(ndemes == 1) %>% PLOTFUN()
lolliplot2 <- data_smr %>% filter(ndemes > 1) %>% PLOTFUN()

############################

# Simulation folders
paths2 <- list.dirs("../data/surviving-founders-traits/", recursive = TRUE)
paths2 <- paths2[str_detect(paths2, "sim-")]

# For each simulation...
data2 <- map_dfr(
  paths2, function(dir) {

    # Read the parameters
    pars <- read_parameters(dir)

    # Read the individual data
    read_individual_data(dir) %>%
      mutate(allfreq = pars$allfreq, selfing = pars$selfing)

  }, .id = "sim"
)

# Format
data2 <- data2 %>% FORMAT()

# Plot
trait_plot <- data2 %>%
  ggplot(aes(x = time / 1000, y = x, color = outcrossing)) +
  geom_point() +
  facet_grid(. ~ allfreq_lab, labeller = label_parsed) +
  scale_color_gradient(low = "gray", high = "gray20") +
  theme(legend.position = "none") +
  xlab(parse(text = "'Time ('*10^3~'generations)'")) +
  ylab("Stress tolerance (x)") +
  ylim(c(0, 10))

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
  c <- 0.2

)

# Generate a PIP
pip <- plot_pip(seq(0, 10, 0.1), model(), pars, extra = TRUE)

# Extract equilibrium density plot
densplot <- pip[[2]]

# Extract PIP
pip <- pip[[3]]

# Update data
densplot$data <- densplot$data %>%
  mutate(
    name = fct_recode(name, "UF" = "N[1]", "F" = "N[2]"),
    name = factor(name, levels = c("F", "UF"))
  )

# Customize
densplot <- densplot +
  aes(linetype = name, color = NULL) +
  labs(linetype = "Patch")

# Combine
P0 <- wrap_plots(densplot, pip, ncol = 1, heights = c(1, 5))

############################

# Plot population size
p1 <- data_smr %>%
  filter(outcrossing %in% c(0, 1), allfreq == 0.2) %>%
  ggplot(aes(x = time, y = n, color = outcrossing)) +
  geom_line() +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = "g") +
  scale_color_gradient(low = "gray", high = "gray20", guide = "legend", breaks = c(0, 1))

# Plot traits
p2 <- data2 %>%
  filter(outcrossing %in% c(0, 1), allfreq == 0.2) %>%
  ggplot(aes(x = time, y = x, color = outcrossing)) +
  geom_point() +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  scale_color_gradient(low = "gray", high = "gray20", guide = "legend", breaks = c(0, 1)) +
  theme(legend.position = "none")

# Combine
zoom_plot <- wrap_plots(p1 + rm_axis("x"), p2, ncol = 1)

############################

# Combine all
P <- wrap_plots(
  lolliplot1,
  lolliplot2,
  wrap_plots(
    P0,
    wrap_plots(trait_plot, zoom_plot, ncol = 1, heights = c(1, 2), guides = "collect"),
    widths = c(5, 6)
  ), ncol = 1, heights = c(2, 2, 6)
) +
  plot_annotation(tag_levels = "A")

# Save (takes a while)
ggsave("plots/surviving_founders.png", P, width = 11, height = 10, dpi = 300)
