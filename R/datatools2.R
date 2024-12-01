#' @export
import_data <- function(filepath, filetype = "csv") {
  if (filetype == "csv") {
    data <- read.csv(filepath)
  } else if (filetype == "xlsx") {
    library(readxl)
    data <- read_excel(filepath)
  } else {
    stop("Unsupported file type. Please use 'csv' or 'xlsx'.")
  }
  return(data)
}

#' @export
summarize_data1 <- function(data) {
  summary_stats <- summary(data)
  missing_values <- colSums(is.na(data))
  return(list(
    summary_stats = summary_stats,
    missing_values = missing_values
  ))
}
