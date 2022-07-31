#' Produces PDF files with avg, max and min of inflammation over all the days.
#'
#' This function takes csv files of patients inflammation and prints pdf files with average,
#' min and max inflammation for each day.
#' @param pattern pattern of the name of the files
#' @return PDF files with graphs
#' @export
#' @examples
#' analyze_all("inflammation*.csv")

analyze_all <- function(pattern) {
  # Directory name containing the data
  data_dir <- "data"
  # Directory name for results
  results_dir <- "results"
  # Runs the function analyze for each file in the given working directory
  # that contains the given pattern.
  filenames <- list.files(path = data_dir, pattern = pattern)
  for (f in filenames) {
    pdf_name <- file.path(results_dir, sub("csv", "pdf", f))
    analyze(file.path(data_dir, f), output=pdf_name)
  }
}
