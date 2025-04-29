# Function to read patch size data
read_patch_size_data <- function(dir) {

  # dir: simulation directory

  # Number of demes
  ndemes <- readsim::read_pars(dir)$ndemes

  # Read times
  time <- readsim::read_data(dir, "time", dupl = 2 * ndemes)[[1]]

  # Read data
  n <- readsim::read_data(dir, "patchsizes")[[1]]

  # Columns of demes and patches
  deme <- rep(1:ndemes, length(time) / ndemes)
  patch <- rep(0:1, length(time) / 2)

  # Combine
  data <- tibble(time, deme, patch, n)

  # Exit
  return(data)

}
