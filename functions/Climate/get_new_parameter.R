# Function to get the new value of a parameter after gradual linear change
get_new_parameter <- function(start, end, after = 1) {
  
  # start: starting value of the parameter
  # end: value of the parameter at the end of the change period
  # after: relative time into the changing period (between 0 and 1)
  
  # Linear change in parameter value
  delta <- after * (end - start)
  
  # Updated value
  return(start + delta)
  
}