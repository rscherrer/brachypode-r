# Function to generate bivariate model specifications
model_biv <- function() {
  
  alist(
    
    # Survival in each habitat
    s1 <- 1 / (1 + exp(a * (theta1 - x))),
    s2 <- 1 / (1 + exp(a * (theta2 - x))),
    
    # Reproduction in each habitat
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
    
    # Derivatives used in the gradient formula
    dr1 <- (1 - N[1] / (c * K1)) * r1,
    dr2 <- (1 - N[2] / ((1 - c) * K2)) * r2,
    ds1 <- a * s1 * (1 - s1),
    ds2 <- a * s2 * (1 - s2),
    
    # Compute the gradient
    G1 <- c * r1 * ds1 + (1 - c) * r2 * ds2,
    G2 <- c * s1 * dr1 + (1 - c) * s2 * dr2
    
  )
  
}
