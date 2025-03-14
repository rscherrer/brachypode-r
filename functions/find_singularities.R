# Function to find singular trait values
find_singularities <- function(

  model, pars, from = 0, to = 10, init = c(1, 1), tend = 100, twostep = TRUE,
  cpp = FALSE, ...

) {
  
  # model: model specifications
  # pars: parameter values
  # from, to: bounds of search space
  # init, tend, twostep, cpp: arguments for demographic equilibrium search

  # Equation for which to find the root(s)
  FUN <- function(x) get_gradient(x, model, pars, init, tend, twostep, cpp)

  # Find the roots of the selection gradient
  find_roots(FUN, from, to, ...)

}
