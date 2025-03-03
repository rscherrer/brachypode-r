remove_duplicates_dist <- function(data, cols, maxdist = 0.1) {
  
  if (!all(map_lgl(data, is.numeric))) 
    stop("All cols must be numeric for averaging")
  
  same <- c(dist(data[cols])) < maxdist
  
  if (!any(same)) return(data)
  
  n <- nrow(data)
  ii <- do.call("c", map2(1:(n - 1), (n - 1):1, ~ rep(.x, .y)))
  jj <- do.call("c", map(1:(n - 1), ~ (1 + .x):n))
  
  ii <- ii[same]
  jj <- jj[same]
  
  l <- list(c(ii[1], jj[1]))
  
  if (sum(same) > 1) {
    
    for (k in 2:sum(same)) {
      
      i <- ii[k]
      j <- jj[k]
      
      belongs <- map_lgl(l, ~ i %in% .x | j %in% .x)
      
      if (any(belongs)) {
        
        grp <- which(belongs)[1]
        l[[grp]] <- c(l[[grp]], c(i, j))
        l[[grp]] <- unique(l[[grp]])
        
      } else {
        
        l <- c(l, list(c(i, j)))
        
      }
    }
  }
  
  map_dfr(l, ~ data[.x, ] %>% summarize(across(everything(), mean, na.rm = TRUE)))
  
}