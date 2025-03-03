model <- function() {
  
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
    M <- matrix(c(c, c, 1 - c, 1 - c), 2, 2, byrow = TRUE),
    
    # Reproduction matrix
    R <- matrix(c(r1, 0, 0, r2), 2, 2, byrow = TRUE),
    
    # Transition matrix
    Lambda <- S %*% M %*% R,
    
    # Invasion fitness
    lambda <- c * r1 * s1 + (1 - c) * r2 * s2,
    
    # Useful derivatives
    dr1 <- epsilon * (N[1] / (c * K1) - 1) * r1,
    dr2 <- epsilon * (N[2] / ((1 - c) * K2) - 1) * r2,
    ds1 <- a * s1 * (1 - s1),
    ds2 <- a * s2 * (1 - s2),
    
    # Selection gradient
    G <- c * (dr1 * s1 + r1 * ds1) + (1 - c) * (dr2 * s2 + r2 * ds2),
    
    # Second derivatives
    ddr1 <- epsilon * (N[1] / (c * K1) - 1) * dr1,
    ddr2 <- epsilon * (N[2] / ((1 - c) * K2) - 1) * dr2,
    dds1 <- a * (1 - 2 * s1) * ds1,
    dds2 <- a * (1 - 2 * s2) * ds2,
    
    # Fitness curvature
    H <- c * (ddr1 * s1 + 2 * dr1 * ds1 + r1 * dds1) +
      (1 - c) * (ddr2 * s2 + 2 * dr2 * ds2 + r2 * dds2)
    
  )
  
}
