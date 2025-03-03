plot_mip_transect <- function(

  par, pos, vals, lab, x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  passon = FALSE, tol = 0.001, field = NULL, model_di = NULL, scale = 0.1,
  grid = NULL, verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue",
  col2 = "red", cpp = FALSE

) {

  data <- tibble(value = vals) %>%
    mutate(mip = map(value, function(value) {

      print(paste(par, "=", value))

      pars[[pos]] <- parse_expr(paste(par, "<-", value))

      plot_mip(
        x, model, pars, init, tend, twostep, passon, tol, field, model_di,
        scale, grid, verbose, refined, lower, plotit = FALSE, as_list = TRUE,
        cpp = cpp
      )

    }))

  data <- data %>% add_labels("value", lab)

  EXTRACT <- function(data, which) {

    data %>%
      mutate(.extr = map(mip, ~ .x[[which]])) %>%
      select(-mip) %>%
      unnest(.extr)

  }

  plot <- data %>%
    EXTRACT("data") %>%
    PLOTMIP() +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  if (!is.null(field)) {

    fdata <- data %>% EXTRACT("field")
    plot <- plot %>% ADDFIELD(fdata, scale)

  }

  if (!is.null(grid)) {

    gdata1 <- data %>% EXTRACT("clines1")
    gdata2 <- data %>% EXTRACT("clines2")

    plot <- plot %>% ADDCLINES(gdata1, gdata2, col1, col2)

  }

  return(plot)

}
