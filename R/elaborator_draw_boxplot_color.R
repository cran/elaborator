#' elaborator_draw_boxplot_color - Color preview function
#'
#'@param x color palette
#'
#'@keywords internal

elaborator_draw_boxplot_color <- function(x){
  on_ex <- graphics::par("mar")
  on.exit(graphics::par(on_ex))
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot(NULL, axes = FALSE, xlim = c(0,1), ylim = c(0,1), yaxs = 'i', xaxs = 'i')
  graphics::rect(xleft = graphics::grconvertX(0,'ndc','user'), xright = graphics::grconvertX(1,'ndc','user'),
       ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1,'ndc','user'),
       border = NA, col = '#eaf1f7')
  graphics::rect(xleft = (0:(length(x) - 1))/length(x), xright = (1:length(x))/length(x),
       ybottom = rep(0, length(x)), ytop = rep(1,length(x)), border = NA, col = x)
}
