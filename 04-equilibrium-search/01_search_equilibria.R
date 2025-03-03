# Here we map the various equilibria we found across parameter space.

rm(list = ls())

library(tidyverse)
library(rlang)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# Parameter values
pars <- alist(

  rmax <- 2,
  epsilon <- 0.4,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 2,
  c <- 0.9

)

# Search equilibria across parameter space
plot <- plot_param_space(
  par = c("theta2", "epsilon", "K2", "c"),
  pos = c(7, 2, 4, 8),
  vals = list(
    theta2 = c(0, 1, 2, 3, 5),
    epsilon = c(0, 0.001, 0.01, 0.1, 0.2, 0.4),
    K2 = c(1800, 1000, 500, 100),
    c = c(0.1, 0.3, 0.5, 0.7, 0.9)
  ),
  lab = c("theta[UF]", "epsilon", "K[UF]", "c"),
  model = model(),
  pars = pars,
  from = 0,
  to = 10,
  n = 20
)

# Relabel
plot <- plot +
  xlab(parse(text = "'Stress in unfacilitated patches ('*theta[UF]*')'")) +
  ylab(parse(text = "'Trade-off ('*epsilon*')'"))

# Save
ggsave("plots/equilibrium_search.png", plot, width = 5.4, height = 3.6, dpi = 300)

# Save the data underlying the plot (will be used later on)
saveRDS(plot$data, "data/data.rds")
