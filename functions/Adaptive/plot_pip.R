# Function to produce a pairwise invasibility plot
plot_pip <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  plotit = TRUE, binary = TRUE, tol = 0.001, extra = FALSE, no_na = TRUE,
  cpp = FALSE

) {
  
  # x: trait values to explore
  # model: the model to study
  # pars: parameter values
  # init, tend, twosteps, cpp: arguments for demographic equilibrium search
  # passon: whether to use equilibrium of adjacent trait value as starting point in demographic equilibrium search
  # plotit: whether to return the plot (instead of the underlying data frame)
  # binary: only show positive versus negative invasion fitness (instead of continuous variation)
  # tol: tolerance parameter for invasion boundaries
  # extra: add a plot of equilibrium densities and selection gradients across trait values
  # no_na: whether to remove NAs from the plot
  
  # If needed...
  if (passon) {

    # Prepare a list of demographic equilibria
    N <- vector("list", length(x))

    # For each trait value...
    for (i in seq(x)) {

      # Search demographic equilibrium
      N[[i]] <- find_equilibrium(x[i], model, pars, init, tend, twostep, cpp)
      
      # Use that equilibrium as starting point for the next trait value
      init <- N[[i]]

    }

  } else {

    # Otherwise search for equilibria for all trait values independently
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

  # If needed...
  if (extra) {

    # Add...
    data <- data %>%
      left_join(

        # ... selection gradients across trait values
        get_gradients(x, model, pars, init = N, fast = TRUE) %>%
          rename(xres = "x")

      )

  }

  # Early exit if we only want the data frame
  if (!plotit) return(data)

  # If invasion boundaries must be plotted...
  if (binary) {

    # Make a pairwise invasibility plot
    plot <- data %>%
      PLOTPIP(tol, no_na) +
      xlim(range(x)) +
      ylim(range(x))

  } else {

    # Otherwise plot invasion fitness heatmap
    plot <- data %>% PLOTINV()

  }

  # If needed...
  if (extra) {

    # Plot demographic equilibrium densities across trait values
    plot2 <- data %>%
      group_by(xres) %>%
      filter(x == all_of(x)[1]) %>%
      PLOTDENS()

    # Setup to color code gradient sign
    ii <- 1:2
    if (!any(data$G < 0)) ii <- 1
    if (!any(data$G > 0)) ii <- 2
    colors <- c("red", "blue")[ii]

    # Plot selection gradients across trait values
    plot3 <- data %>%
      group_by(xres) %>%
      filter(x == all_of(x)[1]) %>%
      PLOTGRAD()

    # Combine plots
    plot <- patchwork::wrap_plots(
      plot3 + rm_axis("x"),
      plot2 + rm_axis("x"),
      plot,
      ncol = 1, heights = c(1, 1, 4)
    )

  }

  return(plot)

}
