#' plotDens
#'
#' plot density of log2FC values from results table
#' @export


plotDens <- function(res,
                     main_title = "",
                     selection_ids = NULL,
                     selection_id_type = "symbol",
                     selection_color = rgb(0.7,0,0.9,1),
                     selection_legend = NULL,
                     xlims = c(-5,5),
                     ylims = c(0,1),
                     x_label = "log2FC"){


        plot(density(res$log2FoldChange),
             main = main_title,
             xlab = x_label,
             xlim = xlims,
             ylim = ylims,
             col = "darkgrey", lwd=2)


        if(!(is.null(selection_ids))){
                lines(density(res$log2FoldChange[res[selection_id_type][,1] %in% selection_ids]),
                      col = selection_color, lwd=2)
        }


        abline(v=0, col="grey32")


        if(!(is.null(selection_legend))){
                legend("topleft", legend = c(selection_legend), bg = "white",
                       col = c(selection_color), pch = 19, cex=1)
        }


}






