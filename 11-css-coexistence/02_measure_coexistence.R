# Here we run a coexistence analysis across parameter space: for each
# parameter combination where two alternative CSSs were found, we look at
# whether the two strategies can coexist.

rm(list = ls())

library(tidyverse)
library(rlang)

for (f in list.files("../functions", full.names = TRUE)) source(f)

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

# Read parameter space data (used later for background image)
bdata <- readRDS("../004-equilibrium-search/data/data.rds")

# Wrangle
colnames(bdata)[1:4] <- c("theta2", "epsilon", "K2", "c")
bdata <- bdata %>% select(theta2, epsilon, K2, c, group)

# Save background data
saveRDS(bdata, "data/bdata.rds")

# For each combination of parameters where two CSSs were found...
data <- bdata %>%
  filter(group == "2 CSS 0 BP") %>%
  select(-group) %>%
  mutate(xeq = pmap(list(theta2, epsilon, K2, c, seq(n())), function(theta2, epsilon, K2, c, i) {

    # Display
    print(paste(i, "/", n()))

    # Update parameters accordingly
    pars[[2]] <- parse_expr(paste("epsilon <-", epsilon))
    pars[[4]] <- parse_expr(paste("K2 <-", K2))
    pars[[7]] <- parse_expr(paste("theta2 <-", theta2))
    pars[[8]] <- parse_expr(paste("c <-", c))

    # Find equilibrium strategies
    xeq <- find_singularities(model(), pars, from = 0, to = 10)

    tibble(xeq)

  })) %>%
  unnest(xeq)

# Save
saveRDS(data, "data/data.rds")
