plot_pip <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  plotit = TRUE, binary = TRUE, tol = 0.001, extra = FALSE, no_na = TRUE,
  cpp = FALSE

) {

  if (passon) {

    N <- vector("list", length(x))

    for (i in seq(x)) {

      N[[i]] <- find_equilibrium(x[i], model, pars, init, tend, twostep, cpp)
      init <- N[[i]]

    }

  } else {

    # Compute demographic equilibria of the different residents
    N <- map(x, find_equilibrium, model, pars, init, tend, twostep, cpp)

  }

  # Create a matrix of trait values and population sizes
  data <- tibble(xres = x, N = N)

  # Expand to all combinations of mutants and residents
  data <- data %>% expand(x = x, nesting(xres, N))

  # Compute invasion fitness
  data <- data %>%
    mutate(
      lambda = pmap_dbl(
        list(x, xres, N),
        ~ get_lambda(..1, ..2, model, pars, ..3, fast = TRUE)
      )
    )

  if (extra) {

    data <- data %>%
      left_join(

        get_gradients(x, model, pars, init = N, fast = TRUE) %>%
          rename(xres = "x")

      )

  }

  # Early exit
  if (!plotit) return(data)

  # Pairwise invasibility plot
  if (binary) {

    plot <- data %>%
      PLOTPIP(tol, no_na) +
      xlim(range(x)) +
      ylim(range(x))

  } else {

    # Actual value of the invasion fitness
    plot <- data %>% PLOTINV()

  }

  if (extra) {

    plot2 <- data %>%
      group_by(xres) %>%
      filter(x == all_of(x)[1]) %>%
      PLOTDENS()

    ii <- 1:2
    if (!any(data$G < 0)) ii <- 1
    if (!any(data$G > 0)) ii <- 2

    colors <- c("red", "blue")[ii]

    plot3 <- data %>%
      group_by(xres) %>%
      filter(x == all_of(x)[1]) %>%
      PLOTGRAD()

    plot <- patchwork::wrap_plots(
      plot3 + rm_axis("x"),
      plot2 + rm_axis("x"),
      plot,
      ncol = 1, heights = c(1, 1, 4)
    )

  }

  return(plot)

}
