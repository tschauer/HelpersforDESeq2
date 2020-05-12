#' plottingMA
#'
#' compare log2FC and baseMean values from results table
#' @export



plottingMA <- function(res,
                       main_title = "",
                       selection_ids = NULL,
                       selection_id_type = "symbol",
                       selection_point_size = 0.5,
                       selection_text_label = FALSE,
                       selection_shadow = FALSE,
                       xlims = c(0, 6),
                       ylims = c(-5,5),
                       x_axis_by = 2,
                       padj_cutoff = 0.01,
                       show_legend = TRUE){



        plot(x = log10(res$baseMean+1),
             y = res$log2FoldChange,
             xlab = "log10 mean counts",
             ylab = "log2 fold change",
             xlim = xlims,
             ylim = ylims,
             col = rgb(0,0,0,0.1), pch=19, cex = 0.25,
             xaxt="n")

        axis(side = 1, at = seq(from = xlims[1], to = xlims[2], by = x_axis_by))

        res.sign <- res[res$padj < padj_cutoff,]

        points(x = log10(res.sign$baseMean+1),
               y = res.sign$log2FoldChange,
               col = rgb(0.8,0,0,0.5), pch = 19, cex = 0.25)

        mtext(text = main_title, side = 3, line = 0.5, adj = 0.5, font = 2)


        for(i in seq_along(selection_ids)){

                selection_id <- grep(paste0("^",selection_ids[i], "$"), res[selection_id_type][,1])

                if(length(selection_id) != 1){next()}

                selection_color <- ifelse(res$padj[selection_id] < padj_cutoff, rgb(0.9,0.6,0), rgb(0.7,0.7,0.7))

                points(log10(res$baseMean[selection_id]+1),
                       res$log2FoldChange[selection_id],
                       col = selection_color, pch = 16,
                       cex = selection_point_size)

                if(selection_shadow){
                        points(log10(res$baseMean[selection_id]+1),
                               res$log2FoldChange[selection_id],
                               col = "black", pch=1, lwd=0.5,
                               cex = selection_point_size)
                }

                if(selection_text_label){

                        yadj <- ifelse(res$log2FoldChange[selection_id] > 0, -0.5, 1)

                        if(selection_shadow){
                                text(log10(res$baseMean[selection_id]+1),
                                     res$log2FoldChange[selection_id],
                                     labels = res[selection_id_type][,1][selection_id], adj = c(0, yadj),
                                     col = "black", font = 2)
                        }


                        text(log10(res$baseMean[selection_id]+1),
                             res$log2FoldChange[selection_id],
                             labels = res[selection_id_type][,1][selection_id], adj = c(0, yadj),
                             col = selection_color)

                }
        }


        abline(h=0, col="grey32")

        if(show_legend){

                legend("topright",
                       legend =  c("labeled significant",
                                   "labeled non-significant"),
                       col = c(rgb(0.9,0.6,0,1),
                               rgb(0.7,0.7,0.7,1)),
                       bg = "white", border = NA, bty = "n",
                       cex = 0.8, pch = 19)

                legend("bottomright",
                       legend =  c("all significant",
                                   "all non-significant"),
                       col = c(rgb(0.8,0,0,1),
                               rgb(0,0,0,1)),
                       bg = "white", border = NA, bty = "n",
                       cex = 0.8, pch = 19)
        }

}
