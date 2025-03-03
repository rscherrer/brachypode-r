remove_duplicates_round <- function(data, cols, precis = 3) {
  
  data %>%
    mutate(across(cols, round, precis)) %>%
    distinct()
  
}