## Here we show the results of the coexistence analysis by overlaying pie charts,
## showing the equilibrium abundance of both morphs (and thus whether they can
## coexist) on top of our parameter space plot.

rm(list = ls())

library(tidyverse)
library(rlang)
library(scatterpie)
library(ggnewscale)

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

# Read background data
bdata <- readRDS("data/bdata.rds")

# Read the data
data <- readRDS("data/data.rds")

# Only keep relevant parameter combinations (where two CSSs were found)
data <- data %>%
  group_by(epsilon, K2, theta2, c) %>%
  filter(n() == 3)

# Wrangle
data <- data %>%
  filter(seq(n()) != 2) %>%
  mutate(name = c("x1", "x2")) %>%
  ungroup() %>%
  pivot_wider(names_from = "name", values_from = "xeq")

# For each parameter combination...
data <- data %>%
  mutate(

    # ... with two stable strategies found...
    N = pmap(
      list(K2, theta2, c, epsilon, x1, x2),
      function(K2, theta2, c, epsilon, x1, x2) {

        # Update parameter accordingly
        pars[[2]] <- parse_expr(paste("epsilon <-", epsilon))
        pars[[4]] <- parse_expr(paste("K2 <-", K2))
        pars[[7]] <- parse_expr(paste("theta2 <-", theta2))
        pars[[8]] <- parse_expr(paste("c <-", c))

        # Find the demographic equilibrium between the two strategies
        find_equilibrium_di(x1, x2, model_di(), pars)

      }
    )
  ) %>%
  unnest(N) %>%
  mutate(name = rep(c("N11", "N12", "N21", "N22"), n() / 4)) %>%
  pivot_wider(names_from = name, values_from = N)

# Add labels
data <- data %>%
  add_labels("K2", "K[UF]") %>%
  add_labels("c", "c")

# Note: with scatterpie, the dimensions of the pies are sensitives to the
# scale of the data along the axes, so they can appear very flat... To go around
# that I rescaled the axes and mapped character labels onto them.

# Overlay scatter-pies on top of the background parameter space mapping
plot <- bdata %>%
  add_labels("K2", "K[UF]") %>%
  add_labels("c", "c") %>%
  ggplot(
    aes(
      x = as.numeric(factor(theta2)),
      y = as.numeric(factor(epsilon))
    )
  ) +
  facet_grid(K2_lab ~ c_lab, labeller = label_parsed) +
  geom_tile(aes(fill = group)) +
  scale_fill_manual(values = c("lightsalmon", "lightblue", "salmon3", "steelblue")) +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_scatterpie(
    aes(
      x = as.numeric(factor(theta2)) + 2, # hack to align both layers properly
      y = as.numeric(factor(epsilon)),
      r = 0.4
    ),
    cols = c("N[11]", "N[12]", "N[21]", "N[22]"),
    data = data %>% rename("N[11]" = "N11", "N[12]" = "N12", "N[21]" = "N21", "N[22]" = "N22")
  ) +
  facet_grid(K2_lab ~ c_lab, labeller = label_parsed) +
  scale_fill_manual(
    values = c("forestgreen", "darkolivegreen3", "gold2", "orange"),
    labels = function(x) parse(text = x)
  ) +
  scale_x_continuous(labels = 0:5, breaks = 0:5, limits = c(-0.5, 5.5)) +
  scale_y_continuous(labels = c(0, 0.001, 0.01, 0.1, 0.2, 0.4), breaks = 0:5, limits = c(-0.5, 5.5)) +
  xlab(parse(text = "theta[UF]")) +
  ylab(parse(text = "epsilon")) +
  labs(fill = NULL)

# Save
ggsave("plots/css_coexistence_scatterpie.png", plot, width = 6, height = 4, dpi = 300)
