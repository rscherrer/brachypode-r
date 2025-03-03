get_par_values <- function(pars) {

  parnames <- map_chr(pars, ~ as.character(.x)[2])
  parvalues <- map(pars, ~ as.numeric(as.character(.x)[3]))
  names(parvalues) <- parnames

  return(parvalues)

}
