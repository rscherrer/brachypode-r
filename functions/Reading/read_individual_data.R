# Function to read individual data
read_individual_data <- function(dir) {

  # dir: simulation directory

  # Read the data and split them into the corresponding columns
  data <- readsim::read_data(dir, variables = "individuals", split = 3)

  # Read population sizes at each time point
  popsizes <- readsim::read_data(dir, "popsize")[[1]]

  # Read the data
  data <- readsim::read_data(
    dir, variables = c("time", "individiuals"),
    dupl = list(popsizes, 1)
  )

  # TODO: Is this actually slow?

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "x")

  # Increment the deme index
  data$deme <- data$deme + 1

  # Exit
  return(data)

}
