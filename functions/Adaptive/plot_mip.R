# Function to produce a mutual invasibility plot
plot_mip <- function(

  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  tol = 0.001, field = NULL, model_di = NULL, scale = 0.1, grid = NULL,
  verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue", col2 = "red",
  plotit = TRUE, as_list = FALSE, cpp = FALSE

) {
  
  # x: trait values to explore
  # model: the model to use
  # pars: parameter values
  # init, tend, twostep, passon, tol, cpp: arguments for pairwise invasibility plot
  # field: trait values at which to evaluate dimorphic selection gradient (NULL if none)
  # model_di: dimorphic model specifications
  # scale: scaling parameter for the tick field of selection gradients
  # grid: trait values at which to look for fitness isoclines (NULL if none)
  # verbose, refined: arguments for isocline search
  # lower: whether to show isoclines on the lower diagonal of the plots
  # col1, col2: colors of the isoclines along both dimensions
  # plotit: whether to return a plot
  # as_list: whether to include selection gradient and fitness isocline data in non-graphical output

  print("Producing MIP...")

  # Make pairwise invasibility plot
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

  # Plot the mutual invasibility plot
  plot <- data %>% PLOTMIP()

  # If needed...
  if (!is.null(field)) {

    # Verbose
    print("Adding field...")

    # Make sure dimorphic model is provided
    if (is.null(model_di)) stop("model_di must be supplied if field is not NULL")

    # For each combination of trait values to explore...
    fdata <- expand_grid(x1 = field, x2 = field) %>%
      left_join(data) %>%
      filter(protected) %>%
      mutate(G = map2(x1, x2, function(x1, x2) {

        # Compute the dimorphic selection gradient
        G <- get_gradient_di(x1, x2, model_di, pars, rep(init, 2), tend, twostep, cpp)
        
        # Rescale it
        G <- G / sqrt(G[1]^2 + G[2]^2)
        
        # Make it into a table
        return(tibble(G1 = G[1], G2 = G[2]))

      })) %>%
      unnest(G)

    # Add a field of tick marks to the plot 
    plot <- plot %>% ADDFIELD(fdata, scale)

  }

  # If needed...
  if (!is.null(grid)) {

    # Verbose
    print("Adding clines...")

    # Check that dimorphic model is provided
    if (is.null(model_di)) stop("model_di must be supplied if field is not NULL")

    # Search for fitness isoclines along both dimensions
    gdata1 <- find_clines(grid, model_di, pars, i = 2, j = 1, init, tend, twostep, verbose, refined, model, tol, toprint = "x", cpp)
    gdata2 <- find_clines(grid, model_di, pars, i = 1, j = 2, init, tend, twostep, verbose, refined, model, tol, toprint = "y", cpp)

    # If needed...
    if (lower) {

      # Swap dimensions to display isoclines on the lower diagonal
      gdata1 <- gdata1 %>% rename(x = "x1", x1 = "x2") %>% rename(x2 = "x")
      gdata2 <- gdata2 %>% rename(x = "x1", x1 = "x2") %>% rename(x2 = "x")

    }

    # Add fitness isoclines to the plot
    plot <- plot %>% ADDCLINES(gdata1, gdata2, col1, col2)

  }

  # If needed...
  if (!plotit) {

    # Return only the data frame underlying the plot
    if (!as_list & is.null(field) & is.null(grid)) return(data)

    # Or combine them with selection gradients and fitness isoclines
    data <- list(data = data)
    if (!is.null(field)) data <- c(data, field = list(fdata))
    if (!is.null(grid)) data <- c(data, clines = list(gdata1, gdata2))

    return(data)

  }

  return(plot)

}
