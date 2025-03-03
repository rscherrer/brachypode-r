plot_mip_grid <- function(

  par1, par2, pos1, pos2, vals1, vals2, lab1, lab2,
  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE,
  passon = FALSE, tol = 0.001, field = NULL, model_di = NULL, scale = 0.1,
  grid = NULL, verbose = TRUE, refined = TRUE, lower = FALSE, col1 = "blue",
  col2 = "red", cpp = FALSE

) {

  data <- expand_grid(

    val1 = vals1,
    val2 = vals2

  ) %>%
    mutate(mip = pmap(list(val1, val2, seq(n())), function(val1, val2, i) {

      print(paste(i, "/", n()))

      pars[[pos1]] <- parse_expr(paste(par1, "<-", val1))
      pars[[pos2]] <- parse_expr(paste(par2, "<-", val2))

      plot_mip(
        x, model, pars, init, tend, twostep, passon, tol, field, model_di,
        scale, grid, verbose, refined, lower, plotit = FALSE, as_list = TRUE,
        cpp = cpp
      )

    }))

  data <- data %>%
    rename(par1 = "val1", par2 = "val2") %>%
    add_labels("par1", lab1) %>%
    add_labels("par2", lab2)

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
    facet_grid(par1_lab ~ par2_lab, labeller = label_parsed)

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
