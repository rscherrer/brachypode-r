# Function to determine mutual invasibility of two trait values
is_mut_inv <- function(

  x1, x2, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, tol = 0.0001,
  cpp = FALSE, fast = FALSE

) {
  
  # x1, x2: the two trait values
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search
  # tol: tolerance to use to check fitness equality
  # fast: whether to treat starting points as known equilibria
  
  # Initialize
  init1 <- init2 <- init

  # If needed...
  if (fast) {

    # Split starting points
    init1 <- init[1:2]
    init2 <- init[2:3]

  }

  # Compute invasion fitnesses
  l1 <- get_lambda(x1, x2, model, pars, init1, tend, twostep, tol, cpp, fast)
  l2 <- get_lambda(x2, x1, model, pars, init2, tend, twostep, tol, cpp, fast)

  # Check that each trait value can invade the other
  if (l1 > 1 & l2 > 1) return(TRUE)

}
