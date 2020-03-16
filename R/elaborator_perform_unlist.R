#' elaborator_perform_unlist - function to unlist
#'
#'@param y number of subjects
#'@param x number of treatments
#'
#'@keywords internal

elaborator_perform_unlist <- function(y, x) unlist(lapply(y, function(y) which(y == x)))
