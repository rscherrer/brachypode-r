# Function to read trait mean data
read_trait_mean_data <- function(dir) {

  # dir: simulation directory

  # Number of demes
  ndemes <- readsim::read_pars(dir)$ndemes

  # Read the data
  data <- readsim::read_data(
    dir,
    variables = c("time", "traitmeans"),
    split = c(1, 2 * ndemes)
  )

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "x")

  # Exit
  return(data)

}
