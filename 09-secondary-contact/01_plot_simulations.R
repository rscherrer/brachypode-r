# Here we plot simulations where morphs can coexist but through migration
# from some other deme in the metapopulation.

rm(list = ls())

library(tidyverse)
library(patchwork)
library(brachypoder)

theme_set(theme_classic())

for (f in list.files("../functions", full.names = TRUE)) source(f)

# For each simulation...
data <- map_dfr(list.dirs("data")[-1], function(dir) {

  # Read parameters
  pars <- read_parameters(dir)

  # Read individual traits through time and append key parameters
  read_individual_data(dir) %>%
    mutate(
      pgood = paste(pars$pgood[-1], collapse = " "),
      allfreq = pars$allfreq
    )

}, .id = "sim")

# Plot the simulations
plot <- data %>%
  filter(allfreq == 0.6, time %% 500 == 0) %>%
  mutate(pgood = str_c("{", str_replace_all(pgood, " ", ", "), "}")) %>%
  add_labels("pgood", "c") %>%
  add_labels("deme", "Deme", sep = " ") %>%
  ggplot(aes(x = time / 1000, y = x, color = factor(deme))) +
  geom_point(data = data %>% filter(allfreq == 0, time %% 500 == 0), color = "gray80") +
  geom_point() +
  scale_alpha_continuous(range = c(0.01, 1)) +
  facet_grid(deme_lab ~ pgood_lab, labeller = label_parsed) +
  xlab(parse(text = "'Time ('*10^3~'generations)'")) +
  ylab("Stress tolerance (x)") +
  theme(legend.position = "none")

# Prepare inset plot data
top_data <- data %>%
  group_by(pgood) %>%
  summarize() %>%
  mutate(sim = seq(n())) %>%
  separate(pgood, into = paste0("deme", 1:5), sep = " ") %>%
  mutate(across(deme1:deme5, as.numeric)) %>%
  pivot_longer(deme1:deme5) %>%
  mutate(deme = as.integer(str_remove(name, "deme")))

# Plot insets
top_plot <- top_data %>%
  ggplot(aes(x = deme, y = value)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ sim) +
  ylab(parse(text = "'Cover ('*c[k]*')'")) +
  xlab("Deme (k)") +
  rm_strips("x")

# Combine plot and insets
P <- wrap_plots(top_plot, plot, ncol = 1, heights = c(1, 5))

# Save
ggsave("plots/secondary_contact.png", P, width = 7, height = 8, dpi = 300)
