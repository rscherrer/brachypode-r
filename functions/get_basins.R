# Extract basins of attraction of equilibria from a MIP 
get_basins <- function(data, maxdist = 0.1, precis = 3, perturb = 0.1) {
  
  # data: the MIP data
  # maxdist: maximum distance between points to say that isoclines cross
  # precis: precision argument for duplicate removal
  # perturb: scale of the perturbation to apply to check for convergence
  
  # Extract the coordinates of the isoclines
  l1 <- with(data$clines1, map2(x1, x2, ~ c(.x, .y)))
  l2 <- with(data$clines2, map2(x1, x2, ~ c(.x, .y)))
  
  # Early exit if needed
  if (length(l1) == 0 | length(l2) == 0) return(NULL)
  
  # Compute the distances between isoclines (each point with each point of the opposite isocline)
  distdata <- expand_grid(l1, l2) %>%
    mutate(i = seq(n())) %>%
    unnest(c(l1, l2)) %>%
    mutate(sq = map2_dbl(l1, l2, ~ (.y - .x)^2)) %>%
    group_by(i) %>%
    summarize(d = sqrt(sq[1] + sq[2]), x1 = l1[1], x2 = l1[2], y1 = l2[1], y2 = l2[2])
  
  # Find the points of opposite isoclines that are very close (those are candidate dimorphic equilibria)
  singdata <- distdata %>% 
    filter(d < maxdist) 
  
  if (nrow(singdata) == 0) return(NULL)
  
  # Approximate the location of the singular strategy
  singdata <- singdata %>% 
    mutate(xeq1 = (x1 + y1) / 2, xeq2 = (x2 + y2) / 2) %>%
    select(xeq1, xeq2)
  
  # Kick out those that might be replicates
  singdata <- remove_duplicates_round(singdata, c("xeq1", "xeq2"), precis)
  
  # Extract the field of dimorphic gradients and append the putative singularities to it
  field <- data$field %>%
    mutate(xeq = list(singdata)) %>%
    unnest(xeq) 
  
  # Compute whether the gradient is bringing the population closer to each singularity
  field <- field %>%
    mutate(
      dbef = sqrt((xeq1 - x1)^2 + (xeq2 - x2)^2),
      daft = sqrt((xeq1 - x1 - perturb * G1)^2 + (xeq2 - x2 - perturb * G2)^2),
      conv = daft < dbef
    )
  
  # Keep the relevant data only
  field <- field %>% select(x1, x2, xeq1, xeq2, conv)
  
  return(field)
  
}