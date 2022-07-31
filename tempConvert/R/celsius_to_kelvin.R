#' Converts Celsius to Kelvin
#'
#' This function converts input temperatures in Celsius to Kelvin.
#' @param temp_C The temperature in Celsius
#' @return The temperature in Kelvin
#' @export
#' @examples
#' celsius_to_kelvin(-273.15)

celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}
