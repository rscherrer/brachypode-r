model2 <- function() {
  
  alist(
    
    # Survival in each habitat
    s1 <- 1 / (1 + exp(a * (theta1 - x))),
    s2 <- 1 / (1 + exp(a * (theta2 - x))),
    
    # Reproduction in each habitat
    d <- -log2(1 - epsilon),
    y <- rmax * ((1 - (x / xmax)^(1 / d))^d),
    
    r1 <- exp(y * (1 - N[1] / (c * K1))),
    r2 <- exp(y * (1 - N[2] / ((1 - c) * K2))),
    
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
    dy <- - rmax * (x^((1 - d) / d) / xmax^(1 / d)) * (1 - (x / xmax)^(1 / d))^(d - 1),
    dr1 <- (1 - N[1] / (c * K1)) * dy * r1,
    dr2 <- (1 - N[2] / ((1 - c) * K2)) * dy * r2,
    ds1 <- a * s1 * (1 - s1),
    ds2 <- a * s2 * (1 - s2),
    
    # Selection gradient
    G <- c * (dr1 * s1 + r1 * ds1) + (1 - c) * (dr2 * s2 + r2 * ds2),
    
    # Second derivatives
    ddy <- dy * ((1 - d) * x^((1 - 2 * d) / d) - x^(2 * (1 - d) / d) / xmax^(1 / d)) / (d * x^((1 - d) / d)),
    ddr1 <- (1 - N[1] / (c * K1)) * (ddy * r1 + dy * dr1),
    ddr2 <- (1 - N[2] / ((1 - c) * K2)) * (ddy * r2 + dy * dr2),
    dds1 <- a * (1 - 2 * s1) * ds1,
    dds2 <- a * (1 - 2 * s2) * ds2,
    
    # Compute the curvature at equilibrium
    H <- c * (ddr1 * s1 + 2 * dr1 * ds1 + r1 * dds1) +
      (1 - c) * (ddr2 * s2 + 2 * dr2 * ds2 + r2 * dds2)
    
  )
  
}
