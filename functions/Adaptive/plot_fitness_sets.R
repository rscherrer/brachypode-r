# Function to plot fitness sets
plot_fitness_sets <- function(

  x, model, model_biv, pars, init = c(1, 1), tend = 100, twostep = FALSE,
  L = 0.5, n = 30, scalex = 0.01, scaley = 0.01, res = 100, tol = 0.001,
  use_grad = FALSE, digits = 2, show_sings = TRUE, show_pip = TRUE, cpp = FALSE

) {

  # x: trait values
  # model: model specifications
  # model_biv: bivariate model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # L: scale of the bivariate selection gradient to plot
  # n: number of points to show when plotting bivariate isoclines
  # scalex, scaley: scaling for the portion of the isoclines to show
  # res: resolution to use when covering the range of trait values in PIPs
  # tol: tolerance to use for equal fitness in plotting bivariate PIPs
  # use_grad: whether to use gradient dimensions to pick plotting windows for isoclines
  # digits: rounding to use in plotting bivariate PIPs
  # show_sings: make sure to show singular strategies as part of the trait values explored
  # show_pip: whether to return bivariate PIPs as well

  # Note: by bivariate we mean that traits trading off against each other are
  # denoted as separate and independent variables.

  # Function the get the fitness component trading off with the main trait
  get_y <- function(x, pars) {

    # Evaluate model parameters
    for (i in seq(pars)) eval(pars[[i]])

    # Use the trade-off formula
    rmax - epsilon * x

  }

  # Note: here that component is fecundity, trading off with stress tolerance.

  # Use that function to compute the trade-off counterpart of each trait value
  y <- get_y(x, pars)

  # Bounds of trait space
  xmin <- min(x)
  xmax <- max(x)
  ymin <- min(y)
  ymax <- max(y)

  # Span along each dimension
  xspan <- xmax - xmin
  yspan <- ymax - ymin

  # Make a table of trait combinations
  data <- tibble(xres = x, yres = y)

  # If needed...
  if (show_sings) {

    # Make a table...
    sings <- tibble(

      # ... with each singularity found
      xres = find_singularities(model, pars, from = xmin, to = xmax, init, tend, twostep, cpp),

      # Compute its bivariate counterpart
      yres = get_y(xres, pars)

    )

    # Add singularities to the data
    data <- data %>% bind_rows(sings)

  }

  # Compute demographic equilibria
  data <- data %>%
    arrange(xres) %>%
    mutate(N = map2(xres, yres, find_equilibrium_biv, model_biv, pars, init, tend, twostep, cpp)) %>%
    mutate(N1 = map_dbl(N, first), N2 = map_dbl(N, last)) %>%
    select(-N)

  # If needed...
  if (show_pip) {

    # For each trait combination...
    pipdata <- data %>%
      mutate(pip = pmap(list(xres, yres, N1, N2), function(xres, yres, N1, N2) {

        # Display
        print(paste0("xres = ", xres, ", yres = ", yres))

        # Make bivariate PIP
        plot_pip_biv(
          x = seq(xmin, xmax, xspan / res),
          y = seq(ymin, ymax, yspan / res),
          xres, yres, model_biv, pars, init = c(N1, N2),
          fast = TRUE, plotit = FALSE
        )

      })) %>%
      unnest(pip)

  }

  # For each trait combination...
  data <- data %>%
    mutate(G = pmap(list(xres, yres, N1, N2), function(xres, yres, N1, N2) {

      # Compute bivariate selection gradient
      get_gradient_biv(xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE)

    })) %>%
    mutate(

      # Extract components
      G1 = map_dbl(G, first),
      G2 = map_dbl(G, last)

    ) %>%
    select(-G) %>%
    mutate(

      # Calculate perturbations
      dx = G1 * L / sqrt(G1^2 + G2^2),
      dy = G2 * L / sqrt(G1^2 + G2^2)

    )

  # For each combination of resident values...
  clines <- data %>%
    mutate(data = pmap(list(xres, yres, dx, dy, N1, N2), function(xres, yres, dx, dy, N1, N2) {

      # Prepare functions to compute bivariate invasion fitness
      f <- function(x, y) get_lambda_biv(x, y, xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE) - 1
      g <- function(y, x) get_lambda_biv(x, y, xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE) - 1

      # If needed...
      if (use_grad) {

        # Set plotting window based on selection gradient
        d <- tibble(
          x = seq(xres, xres + dx, length.out = n),
          y = seq(yres, yres + dy, length.out = n)
        )

      } else {

        # Otherwise sample random points
        d <- tibble(
          x = runif(n, xres - scalex * xspan, xres + scalex * xspan),
          y = runif(n, yres - scaley * yspan, yres + scaley * yspan)
        )

      }

      # Then...
      d %>% mutate(

        # Find null isoclines by finding the root of bivariate invasion fitness function
        newx = map2_dbl(x, y, function(x, y) uniroot_noerr(f, c(x - scalex * xspan, x), y = y, as_na = TRUE, val_only = TRUE)),
        newy = map2_dbl(x, y, function(x, y) uniroot_noerr(g, c(y - scaley * yspan, y), x = x, as_na = TRUE, val_only = TRUE))

      )

    })) %>%
    unnest(data)

  # Combine both dimensions
  clines <- bind_rows(
    with(clines, tibble(xres, yres, x, y = newy)),
    with(clines, tibble(xres, yres, x = newx, y))
  )

  # Plot
  plot <- data %>%
    ggplot(aes(x = xres, y = yres)) +
    geom_path() +
    geom_point(data = clines, aes(x = x, y = y, group = xres, color = xres), size = 0.5) +
    geom_point() +
    scale_color_gradientn(colors = c("forestgreen", "darkolivegreen3", "gold2", "orange")) +
    theme(legend.position = "none") +
    xlab("Stress tolerance (x)") +
    ylab("Reproductive output (y)")

  # If needed...
  if (show_sings) {

    # Add singular strategies as distinguishable points
    plot <- plot +
      geom_point(data = sings, size = 3, shape = 21, fill = "white")

  }

  # Exit now if no PIP needed
  if (!show_pip) return(plot)

  # Internal function for facet labels
  LABEL <- function(data) {

    # data: the data

    # Prepare labels for both traits
    data %>%
      add_labels("xres", "hat(x)") %>%
      add_labels("yres", "hat(y)")

  }

  # Internal function for rounding
  ROUND <- function(data, digits) {

    # data: the data
    # digits: rounding precision

    # Round the trait values
    data %>%
      mutate(xres = round(xres, digits), yres = round(yres, digits))

  }

  # Make PIPs
  plot2 <- pipdata %>%
    ROUND(digits) %>%
    LABEL() %>%
    PLOTPIP2(tol) +
    facet_wrap(. ~ xres_lab + yres_lab, labeller = label_parsed) +
    geom_path(data = data, aes(x = xres, y = yres), color = "white") +
    geom_point(data = data, aes(x = xres, y = yres), color = "white", size = 1) +
    geom_segment(data = data %>% ROUND(digits) %>% LABEL(), aes(x = xres, y = yres, xend = xres + dx, yend = yres + dy)) +
    geom_point(aes(x = xres, y = yres)) +
    xlab("Mutant stress tolerance (x)") +
    ylab("Mutant reproductive output (y)")

  # If needed...
  if (show_sings) {

    # Show singular strategies as distinguishable cases
    plot2 <- plot2 +
      geom_point(
        data = sings %>% ROUND(digits) %>% LABEL(),
        aes(x = xres, y = yres), size = 3, shape = 21, fill = "white"
      )

  }

  # Add fitness isoclines
  plot2 <- plot2 +
    geom_point(
      data = clines %>% ROUND(digits) %>% LABEL(),
      aes(x = x, y = y), color = "red", size = 0.5
    )

  # Return the fitness set plot and the PIPs
  return(list(plot, plot2))

}
