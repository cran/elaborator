#' elaborator_calculate_spearman_distance - function to calculate distance matrix with spearman method
#'
#'@param x data matrix for which the spearman distance should be calculated
#'
#'@keywords internal

elaborator_calculate_spearman_distance <- function(x){
  stats::as.dist(1 - abs(stats::cor(t(x), method = "spearman", use = "complete.obs")))
}
