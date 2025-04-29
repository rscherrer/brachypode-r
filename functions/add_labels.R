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

  # New column name
  col_name <- paste0(col, "_lab")

  # If already present...
  if (col_name %in% colnames(data)) {

    # Identify where
    ii <- which(col_name == colnames(data))

    # Remove column(s)
    data <- data[-ii]

    # Say something
    message("Removed duplicate column")

  }

  # Rename the label column
  colnames(data)[colnames(data) == ".lab"] <- col_name

  return(data)

}
