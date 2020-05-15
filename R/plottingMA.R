#' plottingMA
#'
#' compare log2FC and baseMean values from results table
#' @export



plottingMA <- function(res,
                       main_title = "",
                       point_size = 0.25,
                       point_color =      rgb(0.7,0.7,0.7,0.5),
                       sign_point_color = rgb(0.9,0.6,0.0,0.5),
                       selection_ids = NULL,
                       selection_id_type = "symbol",
                       selection_point_size = 0.5,
                       selection_point_color =      rgb(0,0,0,1),
                       selection_sign_point_color = rgb(0.8,0,0,1),
                       selection_text_label = FALSE,
                       selection_text_size = 1,
                       selection_shadow = FALSE,
                       xlims = c(0, 6),
                       ylims = c(-5,5),
                       x_axis_by = 2,
                       padj_cutoff = 0.01,
                       show_legend = TRUE){

        res$log10baseMean <- log10(res$baseMean+1)

        plot(x = res$log10baseMean,
             y = res$log2FoldChange,
             xlab = "log10 mean counts",
             ylab = "log2 fold change",
             xlim = xlims,
             ylim = ylims,
             col = point_color, pch=19, cex = point_size,
             xaxt="n")

        axis(side = 1, at = seq(from = xlims[1], to = xlims[2], by = x_axis_by))
        abline(h=0, col="grey32")

        res.sign <- res[res$padj < padj_cutoff,]

        points(x = res.sign$log10baseMean,
               y = res.sign$log2FoldChange,
               col = sign_point_color, pch = 19, cex = point_size)

        mtext(text = main_title, side = 3, line = 0.5, adj = 0.5, font = 2)


        if(!(is.null(selection_ids))){

                selection_vector <-  res[selection_id_type][,1] %in% selection_ids

                selection_color <- ifelse(res$padj < padj_cutoff, selection_sign_point_color, selection_point_color)

                points(x = res$log10baseMean[selection_vector],
                       y = res$log2FoldChange[selection_vector],
                       col = selection_color[selection_vector],
                       pch = 16, cex = selection_point_size)

                if(selection_shadow){
                        points(x = res$log10baseMean[selection_vector],
                               y = res$log2FoldChange[selection_vector],
                               col = "black", pch = 1, lwd = 0.75, cex = selection_point_size)
                }

                if(selection_text_label){

                        if(selection_shadow){

                                text(x = res$log10baseMean[selection_vector],
                                     y = res$log2FoldChange[selection_vector],
                                     labels = res[selection_id_type][,1][selection_vector],
                                     col = "black", adj = c(0, -0.5), font = 2,
                                     cex = selection_text_size)
                        }

                        text(x = res$log10baseMean[selection_vector],
                             y = res$log2FoldChange[selection_vector],
                             labels = res[selection_id_type][,1][selection_vector],
                             col = selection_color[selection_vector],
                             adj = c(0, -0.5),
                             cex = selection_text_size)
                }

        }


        if(show_legend){

                legend("topright",
                       legend =  c("labeled significant",
                                   "labeled non-significant"),
                       col = c(selection_sign_point_color,
                               selection_point_color),
                       bg = "white", border = NA, bty = "n",
                       cex = 0.8, pch = 19)

                legend("bottomright",
                       legend =  c("all significant",
                                   "all non-significant"),
                       col = c(sign_point_color,
                               point_color),
                       bg = "white", border = NA, bty = "n",
                       cex = 0.8, pch = 19)
        }

}
