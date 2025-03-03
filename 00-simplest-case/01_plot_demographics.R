# Here we show some demographic dynamics under the model.

rm(list = ls())

library(tidyverse)
library(rlang)
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
# demographic equilibrium because the demographics are oscillatory.

# For different trait values...
plot <- map_dfr(seq(0, 10, 1), function(x) {

  # Simulate the demographics
  iterate(model(), pars, x, tend = 100) %>% mutate(x = x)

}) %>%

  # Wrangle
  pivot_longer(N1:N2) %>%
  mutate(name = fct_recode(name, "F" = "N1", "UF" = "N2")) %>%

  # And plot
  ggplot(aes(x = t, y = 300 * x, height = value, linetype = name, group = interaction(x, name), color = x)) +
  geom_ridgeline(fill = NA) +
  scale_color_gradientn(colors = c("forestgreen", "darkolivegreen3", "gold2", "orange")) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab("Time") +
  ylab("Density") +
  labs(linetype = NULL)

# Save
ggsave("plots/simplest_case_ridges.png", plot, width = 3, height = 3, dpi = 300)
