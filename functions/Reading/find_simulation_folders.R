# Function to find simulation folders
find_simulation_folders <- function(dir, pattern = "sim_") {

  # dir: where to look
  # pattern: pattern to recognize a simulation folder

  # All folders
  dirs <- list.dirs(dir, recursive = TRUE)[-1]

  # Retain some
  dirs <- dirs[grepl(pattern, dirs)]

  # Return
  return(dirs)

}
