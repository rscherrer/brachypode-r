# Function to update a parameter value in a list
update_pars <- function(pars, name, value) {
  
  # pars: list of parameters
  # name: name of the parameter to change
  # value: new value
  
  # Extract parameter names from the list
  names <- map_chr(pars, ~ as.character(.x)[2])
  
  # Error if not found
  if (sum(names == name) == 0) stop("Parameter not found.")
  
  # Locate the parameter
  hit <- which(names == name)[1]
  
  # Note: only the first hit will be changed.
  
  # Update its value 
  pars[[hit]] <- parse_expr(paste(name, "<-", value))
  
  return(pars)
  
}
