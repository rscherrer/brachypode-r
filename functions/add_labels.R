add_labels <- function(data, col, label, sep = " = ") {

  data <- data %>%
    mutate(
      .lab = paste0(label, "*'", sep, get(col), "'"),
      .lab = factor(.lab, levels = paste0(label, "*'", sep, sort(unique(all_of(get(col)))), "'"))
    )

  colnames(data)[colnames(data) == ".lab"] <- paste0(col, "_lab")

  return(data)

}
