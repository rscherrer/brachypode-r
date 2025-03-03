# Here we are comparing different ways of finding the demographic equilibrium
# given some trait and parameter values, mostly because oscillatory dynamics
# can give funny results with regular root-finding or simulations.

rm(list = ls())

library(tidyverse)
library(brachypoder)
library(patchwork)
library(ggridges)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Parameter values
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0,
  K1 <- 2000,
  K2 <- 2000,
  a <- 5,
  theta1 <- 0,
  theta2 <- 0,
  c <- 0.5
  
)

# Note: here we are going to use a root-finding algorithm to find the
# demographic equilibrium because the demographics are oscillatory

# Plot a simulation of the demographics
iterate(model(), pars, x = 9, tend = 100) %>%
  ggplot(aes(x = t, y = N1)) +
  geom_line()

# For different trait values...
plot1 <- map_dfr(seq(0, 10, 1), function(x) {
  
  # Run a demographic simulation
  iterate(model(), pars, x, tend = 100) %>% mutate(x = x)
  
}) %>%
  
  # Wrangle
  pivot_longer(N1:N2) %>%
  mutate(name = fct_recode(name, "F" = "N1", "UF" = "N2")) %>%
  
  # And plot
  ggplot(aes(x = t, y = 300 * x, height = value, linetype = name, group = interaction(x, name), color = x)) +
  geom_ridgeline(fill = NA) +
  scale_color_gradientn(colors = c("forestgreen", "darkolivegreen3", "gold2", "orange"), guide = "legend", breaks = 0:10) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab("Time") +
  ylab("Density") +
  labs(linetype = NULL)

# Produce a PIP based on root-finding
plot2 <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1000, 1000), tend = 0, twostep = FALSE, extra = TRUE)

# Produce a PIP using simulations to find equilibrium
plot3 <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1, 1), tend = 100, twostep = FALSE, extra = TRUE)

# Now root-finding but with bad starting values
plot4 <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1, 1), tend = 0, twostep = FALSE, extra = TRUE)

# Now with root-finding informed with simulations
plot5 <- plot_pip(seq(0, 10, 0.1), model(), pars, init = c(1, 1), tend = 100, twostep = TRUE, extra = TRUE)

# Function to make sure the demographic equilibrium plot is split by patch
FUN <- function(plot) {
  
  plot + aes(linetype = name) +
    scale_color_manual(labels = c("F", "UF"), values = c("gray20", "gray80")) +
    scale_linetype_discrete(labels = c("F", "UF")) +
    labs(linetype = NULL)
  
}

# Split the demographic equilibrium plot by patch
plot2[[2]] <- plot2[[2]] %>% FUN()
plot3[[2]] <- plot3[[2]] %>% FUN()
plot4[[2]] <- plot4[[2]] %>% FUN()
plot5[[2]] <- plot5[[2]] %>% FUN()

# Combine plots
P <- wrap_plots(
  plot1,
  wrap_plots(
    plot2 + geom_abline(slope = 1, intercept = 0) & rm_axis("x"), 
    plot3 + geom_abline(slope = 1, intercept = 0) & theme(legend.position = "none") & rm_axis("x") & ylab(NULL), 
    plot4 + geom_abline(slope = 1, intercept = 0) & theme(legend.position = "none"), 
    plot5 + geom_abline(slope = 1, intercept = 0) & theme(legend.position = "none") & ylab(NULL), 
    guides = "collect", ncol = 2
  ),
  nrow = 1, widths = c(1, 2)
) +
  plot_annotation(tag_levels = "A")

# Save
ggsave("plots/root_finding.png", P, width = 10, height = 8, dpi = 300) 
