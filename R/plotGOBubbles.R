#' plotGObBubbles
#'
#' plot GO bubbles from GO table
#' @export



plotGObBubbles <- function(gt,
                           main_title = "GO"){


        gt <- gt[order(gt$`Sign/Exp`),]

        par(fig = c(0,0.67,0.15,0.85), mar=c(4,20,3,0), mgp = c(2.00,1.00,0))

        plot(y = 1:nrow(gt), ylab = "", yaxt="n",
             x = gt$`Sign/Exp`, xlab = "Fold Enrichment",
             cex = log(gt$Significant), pch = 19, lwd=0,
             col = rgb(0,0,0.5, alpha = (-log10(as.numeric(gt$Fisher.classic)))/(max(-log10(as.numeric(gt$Fisher.classic))))))

        axis(side = 2, at = 1:nrow(gt), labels = gt$Term, las=2, cex.axis=1)

        title(main = main_title, adj = 1)

        par(fig = c(0.67,1.00,0.15,0.85), mar=c(2,2,2,2), new=TRUE)

        plot(0:1,0:1, xlab="",ylab="", type="n", xaxt="n",yaxt="n",bty="n")

        dot_size_quantiles <- quantile( round(gt$Significant/5)*5, prob = c(0,0.5,1))

        legend(0,1, legend = dot_size_quantiles, title = expression(bold("Sign. Gene Count")),
               pch = 19, pt.cex =  log(dot_size_quantiles),  bty = "n",
               y.intersp = 1.5, x.intersp = 1.5)

        pval_quantiles <- quantile( as.numeric(gt$Fisher.classic), prob = c(0,0.25,1))
        pval_quantiles_colors <- rgb(0,0,0.5, alpha = (-log10(as.numeric(pval_quantiles)))/(max(-log10(as.numeric(pval_quantiles)))))


        legend(0,0.75, legend = pval_quantiles, title = expression(bold("P-value Colors")),
               pch = 19, pt.cex =  2, col = pval_quantiles_colors, pt.lwd = 0, bty = "n",
               y.intersp = 1.25, x.intersp = 1.25)
}
