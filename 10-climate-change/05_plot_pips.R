## Here we show how the PIP changes over the course of the climate change
## period for each of the three kinds of scenarios (no macro-mutations cause
## that one is the same as shrub cover shrinkage from an adaptive landscape
## perspective). We add example simulations on top of the plot for
## illustrative purpose.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)
library(patchwork)

theme_set(theme_classic())

source("../functions.R")

# Load C++ code
sourceCpp("../functions/Adaptive/src/iterate.cpp")

# Parameter values
pars0 <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.3
  
)

# Time points
times <- c(0, 0.3, 0.5, 0.6, 0.7, 0.8, 1)

# Make room for multiple parameter sets
pars <- rep(list(pars0), length(times))

# PIP function used in this experiment
this_pip <- function(pars, labels, labels2) {
  
  # pars: list of parameter settings
  # labels, labels2: facet labels 
  
  # Make PIPs across parameter combinations
  plot_pip_across(
    x = seq(0, 10, 0.1),
    model = model(), 
    pars = pars,
    cpp = TRUE,
    labels = labels,
    labels2 = labels2,
    ncol = 1,
    strip.position = "left"
  )
  
}

# Function to plot PIPs in the stress increase scenario
plot_pips_si <- function(pars, times) {
  
  # pars: list of parameter settings
  # times: relative time points during climate change
  
  # Generate changing parameter values
  k1 <- get_new_parameter(2000, 100, after = times)
  theta1 <- get_new_parameter(0, 5, after = times)
  
  # Update parameter values
  pars <- map2(pars, k1, ~ update_pars(..1, "K1", ..2))
  pars <- map2(pars, theta1, ~ update_pars(..1, "theta1", ..2))
  
  # Prepare facet labels
  labels <- paste0("theta[F]*' = ", theta1, ", '*K[F]*' = ", k1, "'")
  labels2 <- NULL
  
  # Plot
  plot <- this_pip(pars, labels, labels2)
  
  return(plot)
  
}

# Function to plot PIPs in the landscape deterioration scenario
plot_pips_ld <- function(pars, times) {
  
  # pars: list of parameter settings
  # times: relative time points during climate change
  
  # Generate changing parameter values
  k1 <- get_new_parameter(2000, 100, after = times)
  k2 <- get_new_parameter(100, 50, after = times)
  theta1 <- get_new_parameter(0, 5, after = times)
  theta2 <- get_new_parameter(5, 7, after = times)
  
  # Update parameter values
  pars <- map2(pars, k1, ~ update_pars(..1, "K1", ..2))
  pars <- map2(pars, k2, ~ update_pars(..1, "K2", ..2))
  pars <- map2(pars, theta1, ~ update_pars(..1, "theta1", ..2))
  pars <- map2(pars, theta2, ~ update_pars(..1, "theta2", ..2))
  
  # Prepare facet labels
  labels <- paste0("theta[F]*' = ", theta1, ", '*K[F]*' = ", k1, "'")
  labels2 <- paste0("theta[UF]*' = ", theta2, ", '*K[UF]*' = ", k2, "'")
  
  # Plot
  plot <- this_pip(pars, labels, labels2)
  
  return(plot)
  
}

# Function to plot PIPs in the cover shrinkage scenario
plot_pips_cs <- function(pars, times) {
  
  # pars: list of parameter settings
  # times: relative time points during climate change
  
  # Generate changing parameter values
  c <- get_new_parameter(0.3, 0.1, after = times)
  
  # Update parameter values
  pars <- map2(pars, c, ~ update_pars(..1, "c", ..2))
  
  # Prepare facet labels
  labels <- paste0("'c = ", c, "'")
  labels2 <- NULL
  
  # Plot
  plot <- this_pip(pars, labels, labels2)
  
  return(plot)
  
}

# Plot
p1 <- plot_pips_si(pars, times)
p2 <- plot_pips_ld(pars, times)
p3 <- plot_pips_cs(pars, times)

# Equilibria (approx.)
eqs1 <- tribble(
  ~ x, ~ i, ~ set,
  1, 1, 1,
  5, 2, 1,
  2.5, 1, 2,
  5.2, 2, 2,
  3.5, 1, 3,
  5.5, 2, 3,
  5.5, 2, 4,
  5.7, 2, 5,
  6, 2, 6
)

# Second plot
eqs2 <- tribble(
  ~ x, ~ i, ~ set,
  1, 1, 1,
  5, 2, 1,
  2.5, 1, 2,
  5.8, 2, 2,
  3.5, 1, 3,
  6.4, 2, 3,
  4, 1, 4,
  6.7, 2, 4,
  4.5, 1, 5,
  6.9, 2, 5,
  5, 1, 6,
  7.3, 2, 6,
  8.2, 2, 7
)

# Third
eqs3 <- tribble(
  ~ x, ~ i, ~ set,
  1, 1, 1,
  5.1, 2, 1,
  1, 1, 2,
  5.5, 2, 2,
  1, 1, 3,
  5.7, 2, 3,
  1, 1, 4,
  5.8, 2, 4,
  1, 1, 5,
  5.9, 2, 5,
  5.9, 2, 6,
  6, 2, 7
)

# Function to complete the table
complete_eqs <- function(eqs) {
  
  # Tweak
  eqs %>% 
    mutate(
      xres = x,
      i = as.factor(i)
    )
  
}

# Function to hack points in
hack_points <- function(plot, points) {
  
  # Hack some points in
  plot +
    geom_point(data = points, aes(fill = NULL, color = i), alpha = 0.6, size = 10) +
    guides(color = "none", fill = guide_legend(override.aes = list(size = 2, color = c("gray20", "gray50", "gray80"))))
  
}

# Add points
p1 <- p1 %>% hack_points(complete_eqs(eqs1 %>% mutate(set_lab = unique(p1$data$set_lab)[set])))
p2 <- p2 %>% hack_points(complete_eqs(eqs2 %>% mutate(set_lab = unique(p2$data$set_lab)[set], set_lab2 = unique(p2$data$set_lab2)[set])))
p3 <- p3 %>% hack_points(complete_eqs(eqs3 %>% mutate(set_lab = unique(p3$data$set_lab)[set])))

# Note: Equilibrium points are manually added based on visual inspection.

# Tweak
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + rm_axis("y") + theme(legend.position = "none")
p3 <- p3 + rm_axis("y")

# Combine
P <- wrap_plots(p1, p2, p3, nrow = 1, guides = "collect")

# Function to import a simulation plot
import_sim_plot <- function(path, ymax = NULL, times = NULL) {
  
  # path: path to the simulation folder
  # ymax: vertical limit
  # times: time points to add
  
  # Read simulation data
  data <- read_individual_data(path)
  
  # Set maximum
  if (is.null(ymax)) ymax <- max(data$x)
  
  # Record expected and realized time
  tmax <- read_parameters(path)$tend
  tend <- max(data$time)
  
  # Did the simulation go extinct?
  is_extinct <- tend < tmax
  
  # Plot setup
  plot <- data %>%
    mutate(patch = if_else(patch == 0, "UF", "F")) %>%
    ggplot(aes(x = time / 1000, y = x)) +
    scale_color_manual(values = c("gray20", "gray80")) +
    ylab("Stress tolerance (x)") +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    labs(color = "Patch") +
    geom_vline(xintercept = 10, linetype = 4) +
    ylim(c(0, ymax)) +
    xlim(c(0, tmax / 1000))
  
  # If extinction...
  if (is_extinct) {
    
    # Add rectangle
    plot <- plot + 
      geom_rect(
        aes(xmin = tend / 1000, x = NULL, y = NULL), 
        data = tibble(tend), 
        xmax = tmax / 1000, 
        ymin = 0, 
        ymax = ymax,
        fill = "gray20", 
        alpha = 0.5
      )
    
  }
  
  # If needed...
  if (!is.null(times)) {
    
    # Start and end of climate change
    from <- read_parameters(path)$tchange
    to <- from + read_parameters(path)$twarming
    
    # Add time points
    plot <- plot +
      geom_vline(
        mapping = aes(xintercept = time, alpha = time), 
        data = tibble(time = (from + times * (to - from)) / 1000)
      ) +
      guides(alpha = "none")
    
  }
  
  # Add points to plot
  plot <- plot + geom_point(aes(color = factor(patch)))
  
  return(plot)
  
}

# Import simulation plots 
p1.1 <- import_sim_plot("../data/climate-change/standard/stress-increase-K2-100/sim-5", ymax = 10, times = times)
p2.1 <- import_sim_plot("../data/climate-change/standard/landscape-deterioration/sim-5", ymax = 10, times = times)
p3.1 <- import_sim_plot("../data/climate-change/standard/cover-shrinkage-K2-100/sim-5", ymax = 10, times = times) 

# Tweak
p2.1 <- p2.1 + rm_axis("y")
p3.1 <- p3.1 + rm_axis("y")

# Titles
p1.1 <- p1.1 + ggtitle("Stress increase\nunder the shrubs") + theme(plot.title = element_text(hjust = 0.5))
p2.1 <- p2.1 + ggtitle("Whole landscape\ndeterioration") + theme(plot.title = element_text(hjust = 0.5))
p3.1 <- p3.1 + ggtitle("Shrub cover\nshrinkage") + theme(plot.title = element_text(hjust = 0.5))

# Combine
P_top <- wrap_plots(p1.1, p2.1, p3.1, nrow = 1, guides = "collect")

# Combine
P_full <- wrap_plots(P_top, P, ncol = 1, heights = c(1, 8))

# Tag
P_full <- P_full + plot_annotation(tag_levels = list(c("A", "B", "C")))

# Save
ggsave("plots/climate_change_pips.png", P_full, width = 7.2, height = 12, dpi = 300)
