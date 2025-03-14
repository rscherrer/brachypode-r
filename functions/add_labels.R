# Function to create a facet label column in a data frame 
add_labels <- function(data, col, label, sep = " = ") {

  # data: the data frame
  # col: name of the column to make labels out of
  # label: label prefix
  # sep: text separator between label prefix and value
  
  # Create the label column
  data <- data %>%
    mutate(
      .lab = paste0(label, "*'", sep, get(col), "'"),
      .lab = factor(.lab, levels = paste0(label, "*'", sep, sort(unique(all_of(get(col)))), "'"))
    )

  # Rename the label column
  colnames(data)[colnames(data) == ".lab"] <- paste0(col, "_lab")

  return(data)

}
