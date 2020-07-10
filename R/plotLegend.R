#' plotLegend
#'
#' Add legend to the bottom of an existing plot
#' @export


plotLegend <- function(conditions,
                       legend_colors,
                       legend_size = 0.8,
                       horizontal = T){


        par(fig=c(0,1,0,1), mar=c(0,0,0,0), oma=c(0,0,0,0), new=TRUE)

        plot.new()

        legend("bottom",
               legend = levels(conditions),
               #text.width = 0.01,
               horiz = horizontal, bty = "n", cex = legend_size, pt.cex = legend_size*1.2,
               pch = 19, col =  legend_colors[seq_along(levels(conditions))])


}
