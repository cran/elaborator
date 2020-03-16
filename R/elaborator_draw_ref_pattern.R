#' elaborator_draw_ref_pattern - create the Reference-value based pattern plots
#' @param pattern_Matrix matrix with reference-value based pattern information
#' @param fontsize font size
#' @param numberColumns number of columns
#' @param ColorBG background color
#'
#' @keywords internal

elaborator_draw_ref_pattern <- function(pattern_Matrix, fontsize, numberColumns, ColorBG = "#E2F3F2") {
  graphics::plot(0, 0,
                 type = "n",
                 axes = FALSE,
                 xlab = "",
                 ylab = "",
                 xlim = c(-1, numberColumns + 0.5),
                 ylim = c(0, 2^numberColumns + 1))
  graphics::rect(xleft = graphics::par()$usr[1] - 0.1,
                 ybottom = graphics::par()$usr[3] - 0.1,
                 xright = graphics::par()$usr[2] + 0.1,
                 ytop = graphics::par()$usr[4] + 0.1,
                 col = ColorBG)

  elaborator_draw_dots(x = 0,
                       y = 1,
                       height = 2^numberColumns,
                       dot_col = "white",
                       pattern_Matrix = pattern_Matrix,
                       number_column = numberColumns,
                       dot_Radius = 0.3,
                       dot_radius = 0.07,
                       fontsize = fontsize)
}
