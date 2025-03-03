plot_mip <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  tol = 0.001, field = NULL, model_di = NULL, scale = 0.1, grid = NULL,
  verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue", col2 = "red",
  plotit = TRUE, as_list = FALSE, cpp = FALSE

) {

  print("Producing MIP...")

  # Make Pairwise Invasibility Plot
  plot <- plot_pip(x, model, pars, init, tend, twostep, passon, tol = tol, no_na = FALSE, cpp = cpp)

  # Extract its data component
  data <- plot$data

  # Prepare mutual invasibility data
  data <- data %>%
    rename(y = "xres", xres = "x") %>%
    rename(x = "y") %>%
    arrange(x, xres) %>%
    mutate(class0 = data$class) %>%
    mutate(protected = class == "> 1" & class0 == "> 1") %>%
    rename(x1 = "x", x2 = "xres")

  # Plot the MIP
  plot <- data %>% PLOTMIP()

  if (!is.null(field)) {

    print("Adding field...")

    if (is.null(model_di)) stop("model_di must be supplied if field is not NULL")

    fdata <- expand_grid(x1 = field, x2 = field) %>%
      left_join(data) %>%
      filter(protected) %>%
      mutate(G = map2(x1, x2, function(x1, x2) {

        G <- get_gradient_di(x1, x2, model_di, pars, rep(init, 2), tend, twostep, cpp)
        G <- G / sqrt(G[1]^2 + G[2]^2)
        return(tibble(G1 = G[1], G2 = G[2]))

      })) %>%
      unnest(G)

    plot <- plot %>% ADDFIELD(fdata, scale)

  }

  if (!is.null(grid)) {

    print("Adding clines...")

    if (is.null(model_di)) stop("model_di must be supplied if field is not NULL")

    gdata1 <- find_clines(grid, model_di, pars, i = 2, j = 1, init, tend, twostep, verbose, refined, model, tol, toprint = "x", cpp)
    gdata2 <- find_clines(grid, model_di, pars, i = 1, j = 2, init, tend, twostep, verbose, refined, model, tol, toprint = "y", cpp)

    if (lower) {

      gdata1 <- gdata1 %>% rename(x = "x1", x1 = "x2") %>% rename(x2 = "x")
      gdata2 <- gdata2 %>% rename(x = "x1", x1 = "x2") %>% rename(x2 = "x")

    }

    plot <- plot %>% ADDCLINES(gdata1, gdata2, col1, col2)

  }

  if (!plotit) {

    if (!as_list & is.null(field) & is.null(grid)) return(data)

    data <- list(data = data)
    if (!is.null(field)) data <- c(data, field = list(fdata))
    if (!is.null(grid)) data <- c(data, clines = list(gdata1, gdata2))

    return(data)

  }

  return(plot)

}
