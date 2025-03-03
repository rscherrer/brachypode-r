plot_fitness_sets <- function(

  x, model, model_biv, pars, init = c(1, 1), tend = 100, twostep = FALSE,
  L = 0.5, n = 30, scalex = 0.01, scaley = 0.01, res = 100, tol = 0.001,
  use_grad = FALSE, digits = 2, show_sings = TRUE, show_pip = TRUE, cpp = FALSE

) {

  get_y <- function(x, pars) {

    for (i in seq(pars)) eval(pars[[i]])
    rmax - epsilon * x

  }

  y <- get_y(x, pars)

  xmin <- min(x)
  xmax <- max(x)
  ymin <- min(y)
  ymax <- max(y)

  xspan <- xmax - xmin
  yspan <- ymax - ymin

  data <- tibble(xres = x, yres = y)

  if (show_sings) {

    sings <- tibble(

      xres = find_singularities(model, pars, from = xmin, to = xmax, init, tend, twostep, cpp),
      yres = get_y(xres, pars)

    )

    data <- data %>% bind_rows(sings)

  }

  data <- data %>%
    arrange(xres) %>%
    mutate(N = map2(xres, yres, find_equilibrium_biv, model_biv, pars, init, tend, twostep, cpp)) %>%
    mutate(N1 = map_dbl(N, first), N2 = map_dbl(N, last)) %>%
    select(-N)

  if (show_pip) {

    pipdata <- data %>%
      mutate(pip = pmap(list(xres, yres, N1, N2), function(xres, yres, N1, N2) {

        print(paste0("xres = ", xres, ", yres = ", yres))

        plot_pip_biv(
          x = seq(xmin, xmax, xspan / res),
          y = seq(ymin, ymax, yspan / res),
          xres, yres, model_biv, pars, init = c(N1, N2),
          fast = TRUE, plotit = FALSE
        )

      })) %>%
      unnest(pip)

  }

  data <- data %>%
    mutate(G = pmap(list(xres, yres, N1, N2), function(xres, yres, N1, N2) {

      get_gradient_biv(xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE)

    })) %>%
    mutate(

      G1 = map_dbl(G, first),
      G2 = map_dbl(G, last)

    ) %>%
    select(-G) %>%
    mutate(

      dx = G1 * L / sqrt(G1^2 + G2^2),
      dy = G2 * L / sqrt(G1^2 + G2^2)

    )

  clines <- data %>%
    mutate(data = pmap(list(xres, yres, dx, dy, N1, N2), function(xres, yres, dx, dy, N1, N2) {

      f <- function(x, y) get_lambda_biv(x, y, xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE) - 1
      g <- function(y, x) get_lambda_biv(x, y, xres, yres, model_biv, pars, init = c(N1, N2), fast = TRUE) - 1

      if (use_grad) {

        d <- tibble(
          x = seq(xres, xres + dx, length.out = n),
          y = seq(yres, yres + dy, length.out = n)
        )

      } else {

        d <- tibble(
          x = runif(n, xres - scalex * xspan, xres + scalex * xspan),
          y = runif(n, yres - scaley * yspan, yres + scaley * yspan)
        )

      }

      d %>% mutate(

        newx = map2_dbl(x, y, function(x, y) uniroot_noerr(f, c(x - scalex * xspan, x), y = y, as_na = TRUE, val_only = TRUE)),
        newy = map2_dbl(x, y, function(x, y) uniroot_noerr(g, c(y - scaley * yspan, y), x = x, as_na = TRUE, val_only = TRUE))

      )

    })) %>%
    unnest(data)

  clines <- bind_rows(
    with(clines, tibble(xres, yres, x, y = newy)),
    with(clines, tibble(xres, yres, x = newx, y))
  )

  plot <- data %>%
    ggplot(aes(x = xres, y = yres)) +
    geom_path() +
    geom_point(data = clines, aes(x = x, y = y, group = xres, color = xres), size = 0.5) +
    geom_point() +
    scale_color_gradientn(colors = c("forestgreen", "darkolivegreen3", "gold2", "orange")) +
    theme(legend.position = "none") +
    xlab("Stress tolerance (x)") +
    ylab("Reproductive output (y)")

  if (show_sings) {

    plot <- plot +
      geom_point(data = sings, size = 3, shape = 21, fill = "white")

  }

  if (!show_pip) return(plot)

  LABEL <- function(data) {

    data %>%
      add_labels("xres", "hat(x)") %>%
      add_labels("yres", "hat(y)")

  }

  ROUND <- function(data, digits) {

    data %>%
      mutate(xres = round(xres, digits), yres = round(yres, digits))

  }

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

  if (show_sings) {

    plot2 <- plot2 +
      geom_point(
        data = sings %>% ROUND(digits) %>% LABEL(),
        aes(x = xres, y = yres), size = 3, shape = 21, fill = "white"
      )

  }

  plot2 <- plot2 +
    geom_point(
      data = clines %>% ROUND(digits) %>% LABEL(),
      aes(x = x, y = y), color = "red", size = 0.5
    )

  return(list(plot, plot2))

}
