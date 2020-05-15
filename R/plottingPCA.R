#' plottingPCA
#'
#' PCA analysis and plotting based on read counts
#' @export






plottingPCA <- function(my_data,
                        xcomp = 1,
                        ycomp = 2,
                        conditions,
                        pca_colors = c("#999999", "#0072B2", "#CC79A7", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442"),
                        main_title = "PCA",
                        quantiles = c(0,1),
                        show_labels = TRUE,
                        point_size = 1.1,
                        my_xlimits = c(-100,100),
                        my_ylimits = c(-100,100)){

        rv <- rowVars(my_data)

        selection <- (rv >  quantile(rv, quantiles[1])  & rv < quantile(rv, quantiles[2]))


        pca <- prcomp(t(my_data[selection, ]), scale. = TRUE)

        percentVar <- round(pca$sdev^2/sum(pca$sdev^2)*100,1)[1:10]


        ##########################################


        plot(pca$x[, xcomp], pca$x[, ycomp]*-1,
             col = pca_colors[conditions],
             pch=16, cex = point_size,
             xlab = paste("PC",xcomp," (", percentVar[xcomp], "%)", sep=""),
             ylab = paste("PC",ycomp," (", percentVar[ycomp], "%)", sep=""),
             xlim= my_xlimits, ylim=my_ylimits)

        points(pca$x[, xcomp], pca$x[, ycomp]*-1,
               cex = point_size, col = "#555555", pch=1, lwd=0.5)

        mtext(text = main_title, side = 3, line = 0.5, adj = 0.5, font = 2)


        if(show_labels){
                text(pca$x[, xcomp], pca$x[, ycomp]*-1, labels = rownames(pca$x),
                     adj = -0.5, col = "gray32", cex=0.5)
        }


        ##########################################

}

