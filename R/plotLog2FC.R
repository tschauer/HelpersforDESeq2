#' plotLog2FC
#'
#' compare log2FC values from two results table
#' @export





plotLog2FC <- function(res1,
                       res2,
                       main_title = "",
                       x_label = "log2FC",
                       y_label = "log2FC",
                       lims = c(-5,5),
                       point_size = 0.25,
                       selection_ids = NULL,
                       selection_id_type = "symbol",
                       selection_color = rgb(0.7,0,0.9,1),
                       selection_point_size = 0.5,
                       selection_legend = NULL,
                       selection_text_label = FALSE){


        res_merged <- merge(res1, res2, by = "row.names")
        colnames(res_merged) <- gsub("symbol.x", "symbol",colnames(res_merged))

        plot(x = res_merged$log2FoldChange.x,
             y = res_merged$log2FoldChange.y,
             main = "",
             xlab = x_label,
             ylab = y_label,
             ylim = lims,
             xlim = lims,
             col = rgb(0,0,0,0.1),
             pch = 19, cex = point_size)

        abline(h=0, v=0, col="grey32")
        abline(coef = c(0,1), col="grey32", lty=2)


        if(!(is.null(selection_ids))){

                points(x = res_merged$log2FoldChange.x[res_merged[selection_id_type][,1] %in% selection_ids],
                       y = res_merged$log2FoldChange.y[res_merged[selection_id_type][,1] %in% selection_ids],
                       col = selection_color,
                       pch = 19, cex = selection_point_size)

                if(selection_text_label){
                        text(x = res_merged$log2FoldChange.x[res_merged[selection_id_type][,1] %in% selection_ids],
                             y = res_merged$log2FoldChange.y[res_merged[selection_id_type][,1] %in% selection_ids],
                             labels = res_merged[selection_id_type][,1][res_merged[selection_id_type][,1] %in% selection_ids],
                             adj = c(0,-0.5),
                             col = selection_color, cex = 1.1)
                }
        }

        if(!(is.null(selection_legend))){
                legend("topleft",
                       legend = c(selection_legend),
                       bg = "white",
                       col = c(selection_color), pch = 19, cex = 1)
        }

}


