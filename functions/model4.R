model4 <- function() {
  
  alist(
    
    # Survival in each habitat
    s1 <- 1 / (1 + exp(a * (theta1 - x))),
    s2 <- 1 / (1 + exp(a * (theta2 - x))),
    
    # Reproduction in each habitat
    r1 <- exp((rmax - epsilon * x) * (1 - N[1] / (c * K1))),
    r2 <- exp((rmax - epsilon * x) * (1 - N[2] / ((1 - c) * K2))),
    
    # Survival matrix
    S <- matrix(c(s1, 0, 0, s2), 2, 2, byrow = TRUE),
    
    # Migration matrix
    M <- matrix(c(
      1 - m, m, 
      m, 1 - m 
    ), 2, 2, byrow = TRUE),
    
    # Reproduction matrix
    R <- matrix(c(r1, 0, 0, r2), 2, 2, byrow = TRUE),
    
    # Transition matrix
    Lambda <- S %*% M %*% R,
    
    # Invasion fitness
    lambda <- 0.5 * ((1 - m) * (r1 * s1 + r2 * s2) +
      sqrt((r1 * s1 + r2 * s2)^2 * (m - 1)^2 - 4 * r1 * r2 * s1 * s2 * (1 - 2 * m)))
      
  )
  
}
