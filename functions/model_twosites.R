model_twosites <- function() {

  alist(

    # Survival in each habitat
    s1 <- 1 / (1 + exp(a * (theta1 - x))),
    s2 <- 1 / (1 + exp(a * (theta2 - x))),

    # Reproduction in site i in habitat j
    r11 <- exp((rmax - epsilon * x) * (1 - N[1] / (c1 * K1))),
    r12 <- exp((rmax - epsilon * x) * (1 - N[2] / ((1 - c1) * K2))),
    r21 <- exp((rmax - epsilon * x) * (1 - N[3] / (c2 * K1))),
    r22 <- exp((rmax - epsilon * x) * (1 - N[4] / ((1 - c2) * K2))),

    # Survival matrix
    S <- matrix(c(
      s1, 0, 0, 0,
      0, s2, 0, 0,
      0, 0, s1, 0,
      0, 0, 0, s2
    ), 4, 4, byrow = TRUE),

    # Migration matrix
    M <- matrix(c(
      (1 - m) * c1, (1 - m) * c1, m * c1, m * c1,
      (1 - m) * (1 - c1), (1 - m) * (1 - c1), m * (1 - c1), m * (1 - c1),
      m * c2, m * c2, (1 - m) * c2, (1 - m) * c2,
      m * (1 - c2), m * (1 - c2), (1 - m) * (1 - c2), (1 - m) * (1 - c2)
    ), 4, 4, byrow = TRUE),

    # Reproduction matrix
    R <- matrix(c(
      r11, 0, 0, 0,
      0, r12, 0, 0,
      0, 0, r21, 0,
      0, 0, 0, r22
    ), 4, 4, byrow = TRUE),

    # Transition matrix
    Lambda <- S %*% M %*% R,

    # Invasion fitness
    lambda <- 0.5 * (
      (1 - m) * (c1 * r11 * s1 + c2 * r21 * s1 + r12 * s2 - c1 * r12 * s2 + r22 * s2 - c2 * r22 * s2) +
        sqrt(
          (m - 1)^2 * (c1 * r11 * s1 + c2 * r21 * s1 + r12 * s2 - c1 * r12 * s2 + r22 * s2 - c2 * r22 * s2)^2 -
            4 * (1 - 2 * m) * (c1 * c2 * r11 * r21 * s1^2 + c2 * r12 * r21 * s1 * s2 - c1 * c2 * r12 * r21 * s1 * s2 + c1 * r11 * r22 * s1 * s2 - c1 * c2 * r11 * r22 * s1 * s2 + r12 * r22 * s2^2 - c1 * r12 * r22 * s2^2 - c2 * r12 * r22 * s2^2 + c1 * c2 * r12 * r22 * s2^2)
        )
    )

  )

}
