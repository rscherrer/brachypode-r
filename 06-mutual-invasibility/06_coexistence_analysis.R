## Here we explore dimorphic coexistence across many parameter combinations,
## and compute some relevant summary statistics for each of those.

rm(list = ls())

library(tidyverse)
library(rlang)
library(Rcpp)

# C++ stuff
sourceCpp("../functions/Adaptive/src/iterate.cpp")
sourceCpp("../functions/Adaptive/src/iterate_di.cpp")

# Load functions
source("../functions.R")

# Parameters
pars <- alist(
  
  rmax <- 2,
  epsilon <- 0.1,
  K1 <- 2000,
  K2 <- 100,
  a <- 5,
  theta1 <- 0,
  theta2 <- 5,
  c <- 0.3
  
)

# Load parameter space mapping
data <- readRDS("../04-equilibrium-search/data/data.rds")

# Keep only a portion of it
data <- data %>% filter(val3 <= 500, val4 <= 0.5)

# Create many combinations of parameters
data <- data %>%
  select(val1, val2, val3, val4) %>%
  mutate(pars = pmap(list(val1, val2, val3, val4), function(val1, val2, val3, val4) {
    
    pars[[7]] <- parse_expr(paste("theta2 <-", val1))
    pars[[2]] <- parse_expr(paste("epsilon <-", val2))
    pars[[4]] <- parse_expr(paste("K2 <-", val3))
    pars[[8]] <- parse_expr(paste("c <-", val4))
    
    return(pars)
    
  }))

# For each one...
data <- data %>%
  mutate(mip = map2(pars, seq(n()), function(pars, i) {
    
    # Display
    print(paste(i, "/", n()))
    
    # Make a MIP
    plot_mip(
      x = seq(0, 10, 0.1), model(), pars, field = seq(0, 10, 0.2),
      grid = seq(0, 10, 0.1), cpp = TRUE, verbose = FALSE, tend = 10000,
      model_di = model_di(), plotit = FALSE
    )
    
  }))

# For each MIP...
data <- data %>%
  mutate(
    
    # Compute the area of mutual invasibility
    area = map_dbl(mip, ~ sum(.x$data$protected, na.rm = TRUE) / nrow(.x$data)),
    
    # And the maximum phenotypic distance between morphs in that area
    maxdiff = map2_dbl(mip, area, ~ ifelse(.y > 0, max(with(filter(.x$data, protected), abs(x1 - x2))), NA))
    
  )

# For each MIP...
data <- data %>%
  mutate(basins = pmap(list(mip, seq(n()), area), function(mip, i, area) {
    
    # Display
    print(paste(i, "/", n()))
    
    # Forget if no morphs can coexist
    if (is.na(area)) return(NULL)
    if (area == 0) return(NULL)
    
    # Compute basins of attraction
    get_basins(mip, maxdist = 0.3, precis = 1, perturb = 0.1)
    
  }))

# For each basin of attraction...
data <- data %>%
  mutate(conv = map(basins, function(data) {
    
    # Forget it if there is no basin of attraction
    if (is.null(data)) return(NULL)
    
    # Compute the convergence area of the basin
    data %>%
      filter(x2 > x1) %>%
      group_by(xeq1, xeq2) %>%
      summarize(conv = sum(conv) / n())
    
  }))

set.seed(22) # for reproducibility

# Pick a number of simulations
nsims <- 5L

# For each MIP...
data <- data %>%
  mutate(xstart = map2(mip, seq(n()), function(mip, i) {
    
    # Forget it if no coexistence possible
    if (sum(mip$data$protected, na.rm = TRUE) == 0) return(tibble(x1 = NA, x2 = NA))
    
    # Keep the trait combinations that can coexist
    mip <- mip$data %>% filter(protected, x2 > x1)
    
    # Sample random starting points for the simulations
    ii <- sample(nrow(mip), min(c(nrow(mip), nsims)))
    
    # Keep those combinations only
    mip <- mip[ii,]
    mip <- mip %>% select(x1, x2)
    
    return(mip)
    
  }))

# For each simulation...
data <- data %>%
  unnest(xstart) %>%
  mutate(sim = pmap(list(x1, x2, pars, seq(n())), function(x1, x2, pars, i) {
    
    # Display
    print(paste("Simulation", i, "/", n()))
    
    # Early exit if non-existent trait value
    if (is.na(x1)) return(tibble(x1end = NA, x2end = NA))
    
    # Prepare empty storage
    data <- NULL
    
    # Set the simulation time (initially zero)
    tend <- 0
    
    # Until we manage to get a complete simulation...
    while (is.null(data)) {
      
      # Try with more generations
      tend <- tend + 500
      
      # Try to...
      data <- tryCatch({
        
        # Run a simulation
        simulate(
          model(), pars, x = c(x1, x2), init = rep(1, 4), tend = tend,
          ntimes = 1000, sigma = 0.5, verbose = FALSE, model_di = model_di(),
          passon = TRUE, cpp = TRUE
        )
        
        # Note: this may result in an error depending on the simulation time.
        # So, we catch the error if there is one and we try again with
        # more generations.
        
      }, error = function(e) NULL)
      
    }
    
    # Keep the last generation
    data %>% 
      filter(time == last(time)) %>% 
      select(x1, x2) %>% 
      rename(x1end = "x1", x2end = "x2")
    
  })) %>%
  unnest(sim)

# Compute the final phenotypic distance between morphs for each simulation
data <- data %>% mutate(diff = abs(x1end - x2end))

# Re-group by parameter combination
data <- data %>%
  group_by(pars) %>%
  nest(sim = c(x1, x2, x1end, x2end, diff))

# For each parameter combination..
data <- data %>%
  mutate(
    smr = map(sim, function(data) {
      
      # Compute some summary statistics across replicate simulations
      with(data, tibble(
        
        # Mean phenotypic distance between the morphs
        meandiff = mean(diff, na.rm = TRUE),
        
        # Variance in said distance among replicates
        vardiff = var(diff, na.rm = TRUE),
        
        # Number of computed distances
        ndiff = sum(!is.na(diff)),
        
        # Number of completed simulations
        nsims = sum(!is.na(x1))
        
      ))
      
    })
  ) %>%
  unnest(smr) %>%
  mutate(
    
    # Add the proportion of simulations for which a distance was computed
    propdiff = ndiff / nsims
    
  )

# Add branching point data
data <- readRDS("../04-equilibrium-search/data/data.rds") %>% 
  select(val1:val4, BP) %>% 
  right_join(data) %>%
  mutate(BP = ifelse(BP, "BP", "No BP"))

# Rename columns
data <- data %>% 
  rename(theta2 = "val1", epsilon = "val2", K2 = "val3", c = "val4")

# Add labels
data <- data %>%
  add_labels("c", "c") %>%
  add_labels("K2", "K[UF]")

# Extra step of duplicate removal (distance-based this time) --- twice
ii <- !map_lgl(data$conv, is.null)
data$conv[ii] <- map(data$conv[ii], remove_duplicates_dist, c("xeq1", "xeq2"), maxdist = 0.3)
data$conv[ii] <- map(data$conv[ii], remove_duplicates_dist, c("xeq1", "xeq2"), maxdist = 0.3)

# We have detected at most one dimorphic equilibrium per parameter set
map_int(data$conv, ~ ifelse(!is.null(.x), nrow(.x), 1L))

# So we can extract a single convergence area per parameter set
data <- data %>%
  mutate(
    
    # Area that converges to a dimorphic coalition
    convarea = map_dbl(conv, ~ ifelse(!is.null(.x), .x$conv[[1]], NA)),
    
    # Phenotypic distance between the morphs at the equilibrium coalition
    ddimcoal = map_dbl(conv, ~ ifelse(!is.null(.x), abs(.x$xeq1 - .x$xeq2), NA))
    
  )

# Save
saveRDS(data, "data/parspace.rds")
