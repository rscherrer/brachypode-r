# Function to extract parameter values
get_par_values <- function(pars) {
  
  # pars: parameters

  # Extract parameter values
  parnames <- map_chr(pars, ~ as.character(.x)[2])
  parvalues <- map(pars, ~ as.numeric(as.character(.x)[3]))
  
  # Name them
  names(parvalues) <- parnames

  return(parvalues)

}
