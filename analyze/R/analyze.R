#' Plots the average, min and max inflammation over time.
#'
#' This function takes a csv file of patients inflammation and outputs average,
#' min and max inflammation for each day.
#' @param filename character string of a csv file
#' @return plots of avg, min and max inflammation
#' @export
#' @examples
#' analyze("inflammation-01.csv")

analyze <- function(filename, output = NULL) {
  # Plot the average, min and max inflammation over time.
  # Input:
  #   filename: character string of a csv file
  #   output: character string of a csv file for saving
  if(!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, type = "l")
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation, type = "l")
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation, type = "l")
  if (!is.null(output)) {
    dev.off()
  }
}
