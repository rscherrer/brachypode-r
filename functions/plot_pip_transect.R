plot_pip_transect <- function(

  par, pos, vals, lab, x, model, pars, init = c(1, 1), tend = 100,
  twostep = TRUE, passon = FALSE, tol = 0.001, extra = TRUE, no_na = TRUE,
  cpp = FALSE

) {

  data <- tibble(value = vals) %>%
    mutate(pip = map(value, function(value) {

      print(paste(par, "=", value))

      pars[[pos]] <- parse_expr(paste(par, "<-", value))

      plot_pip(x, model, pars, init, tend, twostep, passon, extra = extra, no_na = no_na, plotit = FALSE, cpp = cpp)

    })) %>%
    unnest(pip)

  data <- data %>% add_labels("value", lab)

  p1 <- data %>%
    PLOTPIP(tol) +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  if (!extra) return(p1)

  p2 <- data %>%
    group_by(xres, value) %>%
    filter(x == all_of(x)[1]) %>%
    PLOTDENS() +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  p3 <- data %>%
    group_by(xres, value) %>%
    filter(seq(n()) == 1) %>%
    PLOTGRAD() +
    facet_grid(. ~ value_lab, labeller = label_parsed)

  P <- wrap_plots(
    p3 + rm_axis("x"),
    p2 + rm_strips("x") + rm_axis("x"),
    p1 + rm_strips("x"),
    ncol = 1, heights = c(1, 1, 3)
  )

  return(P)

}
