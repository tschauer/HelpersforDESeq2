#' makeCountTable
#'
#' make count table from STAR output
#' @export




makeCountTable <- function(count_names = NULL,
                           count_files,
                           stranded = FALSE){

        if(stranded){
                cidx <- 4
        } else {
                cidx <- 2
        }

        read.counts <- c()

        for(i in seq_along(count_files)){

                tmp <- read.table(count_files[i])

                read.counts <- cbind(read.counts, tmp[,cidx])

        }

        rownames(read.counts) <- tmp[,1]

        if(!is.null(count_names)){colnames(read.counts) <- count_names}

        return(read.counts)
}
