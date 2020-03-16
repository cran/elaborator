#' elaborator_calculate_pattern - function to calculate pattern in ternary respresentation
#'
#' @param index row of combination matrix
#' @param number_combinations length of the combination vector
#'
#'@keywords internal

elaborator_calculate_pattern <- function(index, number_combinations){
  i <- 0
  r <- rep(0, number_combinations)
  while (index > 0) {
    i <- i + 1
    r[i] <- index %% 3
    index <- index %/% 3
  }
  r - 1
}
