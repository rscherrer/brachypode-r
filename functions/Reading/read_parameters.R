# Function to read parameters
read_parameters <- function(dir) {

  # dir: simulation directory

  # Read
  readsim::read_pars(dir, "paramlog.txt", is_bool = c(

    "sow", "loadarch", "savepars", "savearch",
    "savedat", "choose", "verbose"

  ))

}
