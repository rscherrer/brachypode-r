# Function to read individual data
read_individual_data <- function(dir) {

  # dir: simulation directory

  # Read population sizes at each time point
  popsizes <- readsim::read_data(dir, "popsize")[[1]]

  # Read the data
  data <- readsim::read_data(
    dir, variables = c("time", "individuals"),
    dupl = list(popsizes, 1),
    split = c(1, 3)
  )

  # TODO: Is this actually slow?

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "x")

  # Increment the deme index
  data$deme <- data$deme + 1

  # Exit
  return(data)

}
