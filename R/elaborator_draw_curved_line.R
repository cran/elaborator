#' elaborator_draw_curved_line - Draw a curved line from start to end point
#'
#' @param x1 x-coordinate of the starting point
#' @param x2 x-coordinate of the end point
#' @param y1 y-coordinate of the starting point
#' @param y2 y-coordinate of the end point
#' @param ... further parameter
#'
#' @keywords internal

elaborator_draw_curved_line <- function(x1, y1, x2, y2, ...) {

  A <- matrix(c(x1, x2, 1, 1), nrow = 2, ncol = 2)
  b <- c(-5, 5)
  z <- solve(A, b)

  x <- seq(x1, x2, 0.05)
  y <- (y2 - y1) / 2 * tanh(z[1] * x + z[2]) + (y1 + y2) / 2
  graphics::points(x, y, type = "l", ...)
}
