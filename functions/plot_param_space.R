# Function to plot evolutionary equilibria across parameter space
plot_param_space <- function(

  par, pos, vals, lab, model, pars, from = 0, to = 10, init = c(1, 1),
  tend = 100, twostep = TRUE, dx = 0.001, tol = 0.01, plotit = TRUE, cpp = FALSE,
  ...

) {

  # par: names of the parameters to vary
  # pos: respective positions in parameter list
  # vals: lists of values to try
  # lab: prefixes in facet labels
  # model: model specifications
  # pars: parameter values
  # from, to: bounds of trait space
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # dx: perturbation to use in convergence checking
  # tol: tolerance for closeness of demographic equilibria to zero
  # plotit: whether to return a plot instead of data
  # ...: extra arguments for singularity search
  
  # Make combinations of parameter values
  data <- expand_grid(

    val1 = vals[[1]],
    val2 = vals[[2]],
    val3 = vals[[3]],
    val4 = vals[[4]]

  ) %>%
    mutate(

      # And for each of them...
      data = pmap(list(val1, val2, val3, val4, seq(n())), function(val1, val2, val3, val4, i) {

        # Display progress
        print(paste(i, "/", n()))

        # Combine parameter values
        val <- c(val1, val2, val3, val4)

        # Update parameter values
        for (j in seq(val)) pars[[pos[j]]] <- parse_expr(paste(par[j], "<-", val[j]))

        # Make a table out of...
        tibble(

          # The singularities found
          x = find_singularities(model, pars, from, to, init, tend, twostep, cpp, ...),
          
          # Their associated demographic equilibria
          N = map(x, find_equilibrium, model, pars, init, tend, twostep, cpp),
          
          # Whether they are evolutionarily stable
          stable = map_lgl(x, is_stable, model, pars, init, tend, twostep, cpp = cpp),
          
          # Whether they are convergence stable
          convergent = map_lgl(x, is_convergent, model, pars, init, tend, twostep, dx, cpp = cpp),
          
          # Extract demographic equilibria
          N1 = map_dbl(N, first),
          N2 = map_dbl(N, last)

        ) %>% select(-N)

      })

    ) %>% unnest(data)

  # With that data...
  data <- data %>%
    
    # Remove false (i.e. extinct) equilibria
    filter(!(N1 < tol & N2 < tol), N1 > -tol, N2 > -tol) %>%
    group_by(val1, val2, val3, val4) %>%
    summarize(
      
      # Count singularities
      n = n(),
      
      # How many are convergent stable strategies?
      CS = sum(stable & convergent),
      
      # How many are branching points?
      BP = sum(!stable & convergent),
      
      # How many are gardens of Eden?
      GE = sum(stable & !convergent),
      
      # How many are repellors?
      RP = sum(!stable & !convergent)
      
    ) %>% ungroup()

  # Return just the data if needed
  if (!plotit) return(data)

  # Otherwise plot heatmap
  data %>% PLOTPAR(lab)

}
