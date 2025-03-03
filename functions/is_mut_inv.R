is_mut_inv <- function(

  x1, x2, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, tol = 0.0001,
  cpp = FALSE, fast = FALSE

) {

  init1 <- init2 <- init

  if (fast) {

    init1 <- init[1:2]
    init2 <- init[2:3]

  }

  l1 <- get_lambda(x1, x2, model, pars, init1, tend, twostep, tol, cpp, fast)
  l2 <- get_lambda(x2, x1, model, pars, init2, tend, twostep, tol, cpp, fast)

  if (l1 > 1 & l2 > 1) return(TRUE)

}
