## Here we show how the PIP changes over the course of the climate change
## period for each of the three kinds of scenarios (no macro-mutations cause
## that one is the same as shrub cover shrinkage from an adaptive landscape
## perspective). We add example simulations on top of the plot for
## illustrative purpose. Note: this is similar to the same script as the one
## we used for the same purpose in the previous climate change analysis.

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
times <- c(0, 0.3, seq(0.5, 1, 0.1))

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

# Function to plot PIPs in the landscape deterioration scenario
plot_pips_ld <- function(pars, times, k2End = 50, theta2End = 7) {
  
  # pars: list of parameter settings
  # times: relative time points during climate change
  # k2End: end value of K2
  # theta2End: end value of theta2
  
  # Generate changing parameter values
  k1 <- get_new_parameter(2000, 100, after = times)
  k2 <- get_new_parameter(100, k2End, after = times)
  theta1 <- get_new_parameter(0, 5, after = times)
  theta2 <- get_new_parameter(5, theta2End, after = times)
  
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

# Plot
pips <- map2(
  c(50, 62.5, 75, 87.5),
  c(7, 6.5, 6, 5.5),
  ~ plot_pips_ld(pars, times, k2End = ..1, theta2End = ..2)
)

# Stress increase scenario
pip_si <- plot_pips_si(pars, times)

# Combine
pips <- c(pips, list(pip_si))

# Tweak
for (i in 1:(length(pips) - 1)) pips[[i]] <- pips[[i]] + theme(legend.position = "none")
for (i in 2:length(pips)) pips[[i]] <- pips[[i]] + rm_axis("y")

# Combine
P <- wrap_plots(pips, nrow = 1, guides = "collect")

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

# Paths to simulation folders
paths <- list.dirs("../data/climate-change-paceing", full.names = TRUE)[-1]

# Import simulation plots 
simplots <- map(paths, import_sim_plot, ymax = 10, times = times)

# Tweak
for (i in 2:length(simplots)) simplots[[i]] <- simplots[[i]] + rm_axis("y")

# Titles
simplots[[1]] <- simplots[[1]] + ggtitle("Whole landscape\ndeterioration") + theme(plot.title = element_text(hjust = 0.5))
simplots[[2]] <- simplots[[2]] + ggtitle("Slightly slower\ndeterioration") + theme(plot.title = element_text(hjust = 0.5))
simplots[[3]] <- simplots[[3]] + ggtitle("Even slower\ndeterioration") + theme(plot.title = element_text(hjust = 0.5))
simplots[[4]] <- simplots[[4]] + ggtitle("Almost no\ndeterioration") + theme(plot.title = element_text(hjust = 0.5))
simplots[[5]] <- simplots[[5]] + ggtitle("Only\nunder the shrubs") + theme(plot.title = element_text(hjust = 0.5))

# Combine
P_top <- wrap_plots(simplots, nrow = 1, guides = "collect")

# Combine
P_full <- wrap_plots(P_top, P, ncol = 1, heights = c(1, 8))

# Save
ggsave("plots/climate_change_paceing_pips.png", P_full, width = 12, height = 15, dpi = 300)
