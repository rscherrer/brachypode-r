# Function to remove duplicates based on rounding
remove_duplicates_round <- function(data, cols, precis = 3) {
  
  # data: a data frame
  # cols: columns to look across
  # precis: rounding precision to use
  
  # Remove duplicates
  data %>%
    mutate(across(cols, round, precis)) %>%
    distinct()
  
}