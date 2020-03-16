#' elaborator_calculate_pattern_number - adds one to the pattern for a ternary representation
#'
#'@param p pattern
#'
#'@keywords internal

elaborator_calculate_pattern_number <- function(p) {
  . <- NULL
  as.vector((as.matrix(p + 1) %*% as.matrix(3^(0:(dim(p)[2] - 1)))))
}

