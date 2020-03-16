#' elaborator_calculate_color_index - calculates the index of the color vector in the qualitative trends plot
#'
#'@param r numeric value
#'
#'@keywords internal

elaborator_calculate_color_index <- function(r) {
  if (is.na(r)) {
    return(NA)
  }
  h <- floor(0.2 * r) + 1
  if (h <= 11) h else 11
}
