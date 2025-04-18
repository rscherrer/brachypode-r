# Function to read patch size data
read_patch_size_data <- function(dir) {

  # dir: simulation directory

  # Number of demes
  ndemes <- readsim::read_pars(dir)$ndemes

  # Read the data
  data <- readsim::read_data(
    dir,
    variables = c("time", "patchsizes"),
    split = c(1, 2 * ndemes)
  )

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "n")

  # Exit
  return(data)

}
