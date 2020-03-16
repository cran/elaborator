#' elaborator_calculate_ref_pattern - returns a specific row of a combination matrix
#'
#' @param index row of combination matrix
#' @param number_combinations length of the combination vector
#'
#' @keywords internal

elaborator_calculate_ref_pattern <- function(index, number_combinations) {
  tmp <- rep(0, number_combinations)
  while (index > 0) {
    tmp[number_combinations] <- index %% 2
    index <- index %/% 2
    number_combinations <- number_combinations - 1
  }
  tmp
}
