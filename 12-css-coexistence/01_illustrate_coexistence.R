## Here we explore the demographics of tolerant and sensitive morph when grown
## together under a particular parameter combination and various initial densities
## of both morphs.

rm(list = ls())

library(tidyverse)

theme_set(theme_classic())

source("../functions.R")

# Parameter values
pars <- alist(

  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 500,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.5

)

# Plot a PIP
pip <- plot_pip(seq(0, 10, 0.1), model(), pars)

# Find equilibrium strategies
xeq <- find_singularities(model(), pars, from = 0, to = 10)

# For different combinations of initial densities...
data <- expand_grid(
  N1 = c(1, 100),
  N2 = c(1, 100)
) %>%
  add_row(N1 = 0, N2 = 1) %>%
  add_row(N1 = 1, N2 = 0) %>%
  mutate(sim = map2(N1, N2, function(N1, N2) {

    # Run demographic simulations with two morphs
    iterate_di(model_di(), pars, xeq[1], xeq[2], tend = 100, rep(N1, 2), rep(N2, 2))

  })) %>%
  unnest(sim)

# Wrangle
data <- data %>%
  mutate(N1 = 2 * N1, N2 = 2 * N2) %>%
  add_labels("N1", "N['sens,'*0]") %>%
  add_labels("N2", "N['tol,'*0]") %>%
  pivot_longer(N11:N22) %>%
  mutate(
    patch = if_else(name %in% c("N11", "N21"), "F", "UF"),
    x = if_else(name %in% c("N11", "N12"), "'Sensitive morph'", "'Tolerant morph'")
  )

# Plot demographics through time
plot <- data %>%
  ggplot(aes(x = t, y = value, linetype = patch, color = interaction(patch, x))) +
  geom_line() +
  facet_grid(N1_lab + N2_lab ~ x, labeller = label_parsed) +
  scale_color_manual(
    values = c("forestgreen", "darkolivegreen3", "gold2", "orange")
  ) +
  ylab("Density") +
  xlab("Time") +
  labs(linetype = NULL) +
  guides(color = "none")

# Save
ggsave("plots/css_coexistence_example.png", plot, width = 4, height = 6, dpi = 300)
