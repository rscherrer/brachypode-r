plot_pip_grid <- function(

  par1, par2, pos1, pos2, vals1, vals2, lab1, lab2,
  x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, passon = FALSE,
  tol = 0.001, no_na = TRUE, cpp = FALSE

) {

  data <- expand_grid(

    val1 = vals1,
    val2 = vals2

  ) %>%
    mutate(pip = pmap(list(val1, val2, seq(n())), function(val1, val2, i) {

      print(paste(i, "/", n()))

      pars[[pos1]] <- parse_expr(paste(par1, "<-", val1))
      pars[[pos2]] <- parse_expr(paste(par2, "<-", val2))

      plot_pip(x, model, pars, init, tend, twostep, passon, tol = tol, extra = FALSE, plotit = FALSE, cpp = cpp)

    })) %>%
    unnest(pip)

  data <- data %>%
    rename(par1 = "val1", par2 = "val2") %>%
    add_labels("par1", lab1) %>%
    add_labels("par2", lab2)

  plot <- data %>%
    PLOTPIP(tol) +
    xlim(range(x)) +
    ylim(range(x)) +
    facet_grid(par1_lab ~ par2_lab, labeller = label_parsed)

  return(plot)

}
