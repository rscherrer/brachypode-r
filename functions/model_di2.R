# Function to generate dimorphic model specifications with non-linear trade-off
model_di2 <- function() {
  
  # x1: mutant trait value in morph 1
  # x2: mutant trait value in morph 2
  # sij or rij refers to morph i in habitat j
  
  alist(
    
    # Survival functions
    s11 <- 1 / (1 + exp(a * (theta1 - x1))),
    s12 <- 1 / (1 + exp(a * (theta2 - x1))),
    s21 <- 1 / (1 + exp(a * (theta1 - x2))),
    s22 <- 1 / (1 + exp(a * (theta2 - x2))),
    
    # Reproduction functions
    d <- -log2(1 - epsilon),
    y1 <- rmax * ((1 - (x1 / xmax)^(1 / d))^d),
    y2 <- rmax * ((1 - (x2 / xmax)^(1 / d))^d),
    r11 <- exp(y1 * (1 - (N1[1] + N2[1]) / (c * K1))),
    r12 <- exp(y1 * (1 - (N1[2] + N2[2]) / ((1 - c) * K2))),
    r21 <- exp(y2 * (1 - (N1[1] + N2[1]) / (c * K1))),
    r22 <- exp(y2 * (1 - (N1[2] + N2[2]) / ((1 - c) * K2))),
    
    # Survival matrices
    S1 <- matrix(c(s11, 0, 0, s12), 2, 2, byrow = TRUE),
    S2 <- matrix(c(s21, 0, 0, s22), 2, 2, byrow = TRUE),
    
    # Migration matrices
    M <- matrix(c(c, c, 1 - c, 1 - c), 2, 2, byrow = TRUE),
    
    # Reproduction matrices
    R1 <- matrix(c(r11, 0, 0, r12), 2, 2, byrow = TRUE),
    R2 <- matrix(c(r21, 0, 0, r22), 2, 2, byrow = TRUE),
    
    # Transition matrices for each morph
    Lambda1 <- S1 %*% M %*% R1,
    Lambda2 <- S2 %*% M %*% R2,
    
    # Derivatives used in the gradient formula
    dy1 <- - rmax * (x1^((1 - d) / d) / xmax^(1 / d)) * (1 - (x1 / xmax)^(1 / d))^(d - 1),
    dy2 <- - rmax * (x2^((1 - d) / d) / xmax^(1 / d)) * (1 - (x2 / xmax)^(1 / d))^(d - 1),
    dr11 <- (1 - (N1[1] + N2[1]) / (c * K1)) * dy1 * r11,
    dr12 <- (1 - (N1[2] + N2[2]) / ((1 - c) * K2)) * dy1 * r12,
    dr21 <- (1 - (N1[1] + N2[1]) / (c * K1)) * dy2 * r21,
    dr22 <- (1 - (N1[2] + N2[2]) / ((1 - c) * K2)) * dy2 * r22,
    ds11 <- a * s11 * (1 - s11),
    ds12 <- a * s12 * (1 - s12),
    ds21 <- a * s21 * (1 - s21),
    ds22 <- a * s22 * (1 - s22),
    
    # Selection gradient
    G1 <- c * (dr11 * s11 + r11 * ds11) + (1 - c) * (dr12 * s12 + r12 * ds12),
    G2 <- c * (dr21 * s21 + r21 * ds21) + (1 - c) * (dr22 * s22 + r22 * ds22)
    
  )
}
