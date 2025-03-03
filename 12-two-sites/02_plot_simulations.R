# Here we plot some corresponding simulations.

rm(list = ls())

library(tidyverse)
library(brachypoder)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# For each simulation...
data <- map_dfr(list.dirs("data")[-1], function(dir) {

  # Read the parameters
  pars <- read_parameters(dir)

  # Read trait values
  read_individual_data(dir) %>%
    mutate(
      nsites = pars$pgood[1],
      capacityUF = pars$capacities[1],
      deltac = ifelse(nsites == 2, diff(pars$pgood[-1]), NA),
      pgood1 = pars$pgood[2]
    )

}, .id = "sim")

# Plot the simulations
plot <- data %>%
  filter(time %% 500 == 0) %>%
  add_labels("nsites", "n[D]") %>%
  add_labels("deltac", "Delta*c") %>%
  add_labels("capacityUF", "K[UF]") %>%
  add_labels("pgood1", "c[1]") %>%
  ggplot(aes(x = time, y = x, color = factor(deme))) +
  geom_point() +
  facet_grid(nsites_lab + deltac_lab + pgood1_lab ~ capacityUF_lab, labeller = label_parsed) +
  xlab("Time (generations)") +
  ylab("Stress tolerance (x)") +
  labs(color = "Deme")

# Save
ggsave("plots/two_sites.png", plot, width = 6, height = 6, dpi = 300)
