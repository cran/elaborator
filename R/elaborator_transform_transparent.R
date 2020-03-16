#' elaborator_transform_transparent - Calculate the transparent version of a given color
#'
#'@param some_color A color
#'@param alpha shading parameter
#'
#'@keywords internal

elaborator_transform_transparent <- function(some_color, alpha = 100){
  newColor <- grDevices::col2rgb(some_color)
  apply(newColor, 2, function(curcoldata){grDevices::rgb(red = curcoldata[1], green = curcoldata[2],
                                              blue = curcoldata[3], alpha = alpha, maxColorValue = 255)})
}
