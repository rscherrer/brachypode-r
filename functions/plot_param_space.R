plot_param_space <- function(

  par, pos, vals, lab, model, pars, from = 0, to = 10, init = c(1, 1),
  tend = 100, twostep = TRUE, dx = 0.001, tol = 0.01, plotit = TRUE, cpp = FALSE,
  ...

) {

  data <- expand_grid(

    val1 = vals[[1]],
    val2 = vals[[2]],
    val3 = vals[[3]],
    val4 = vals[[4]]

  ) %>%
    mutate(

      data = pmap(list(val1, val2, val3, val4, seq(n())), function(val1, val2, val3, val4, i) {

        print(paste(i, "/", n()))

        val <- c(val1, val2, val3, val4)

        for (j in seq(val)) pars[[pos[j]]] <- parse_expr(paste(par[j], "<-", val[j]))

        tibble(

          x = find_singularities(model, pars, from, to, init, tend, twostep, cpp, ...),
          N = map(x, find_equilibrium, model, pars, init, tend, twostep, cpp),
          stable = map_lgl(x, is_stable, model, pars, init, tend, twostep, cpp = cpp),
          convergent = map_lgl(x, is_convergent, model, pars, init, tend, twostep, dx, cpp = cpp),
          N1 = map_dbl(N, first),
          N2 = map_dbl(N, last)

        ) %>% select(-N)

      })

    ) %>% unnest(data)

  data <- data %>%
    filter(!(N1 < tol & N2 < tol), N1 > -tol, N2 > -tol) %>%
    group_by(val1, val2, val3, val4) %>%
    summarize(
      n = n(),
      CS = sum(stable & convergent),
      BP = sum(!stable & convergent),
      GE = sum(stable & !convergent),
      RP = sum(!stable & !convergent)
    ) %>% ungroup()

  if (!plotit) return(data)

  data %>% PLOTPAR(lab)

}
