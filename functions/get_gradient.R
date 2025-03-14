# Function to compute the selection gradient for a given resident trait value
get_gradient <- function(

    x, model, pars, init = c(1, 1), tend = 100, twostep = TRUE, cpp = FALSE,
    fast = FALSE

) {
  
  # x: resident trait value
  # model: model specifications
  # pars: parameter values
  # init, tend, twostep, cpp: arguments for demographic equilibrium search 
  # fast: whether to treat starting point as a known equilibrium (avoids searching)

  # Unpack the parameters
  for (i in seq(pars)) eval(pars[[i]])

  # Find the demographic equilibrium if needed
  if (fast) N <- init else N <- find_equilibrium(x, model, pars, init, tend, twostep, cpp)

  # Evaluate the model at equilibrium
  for (i in seq(model)) eval(model[[i]])

  # Return the selection gradient
  return(G)

}
