# Function to remove duplicates based on distance
remove_duplicates_dist <- function(data, cols, maxdist = 0.1) {
  
  # data: a data frame
  # cols: columns for coordinates
  # maxdist: maximum distance to be considered duplicate
  
  # Sanity check
  if (!all(map_lgl(data, is.numeric))) 
    stop("All cols must be numeric for averaging")
  
  # Figure which pairs of points fall under the maximum allowed distance
  same <- c(dist(data[cols])) < maxdist
  
  # Early exit if none found
  if (!any(same)) return(data)
  
  # Count data points 
  n <- nrow(data)
  
  # Prepare pairwise comparison indices
  ii <- do.call("c", map2(1:(n - 1), (n - 1):1, ~ rep(.x, .y)))
  jj <- do.call("c", map(1:(n - 1), ~ (1 + .x):n))
  
  # Keep putative duplicates
  ii <- ii[same]
  jj <- jj[same]
  
  # Prepare a list of non-duplicates
  l <- list(c(ii[1], jj[1]))
  
  # If more than one pair...
  if (sum(same) > 1) {
    
    # For each pair...
    for (k in 2:sum(same)) {
      
      # Identify
      i <- ii[k]
      j <- jj[k]
      
      # Is it already counted?
      belongs <- map_lgl(l, ~ i %in% .x | j %in% .x)
      
      # If so...
      if (any(belongs)) {
        
        # Which group of pairwise indices is it in? 
        grp <- which(belongs)[1]
        
        # Add it to that group
        l[[grp]] <- c(l[[grp]], c(i, j))
        
        # Make sure to remove identical entries
        l[[grp]] <- unique(l[[grp]])
        
      } else {
        
        # Otherwise just add the new pair to the list
        l <- c(l, list(c(i, j)))
        
      }
    }
  }
  
  # Return means across duplicates from the original data frame
  map_dfr(l, ~ data[.x, ] %>% summarize(across(everything(), mean, na.rm = TRUE)))
  
}