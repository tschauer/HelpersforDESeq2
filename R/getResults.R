#' getResults
#'
#' get results for comparisons
#' @export





getResults <- function(dds,
                       contrast,
                       result_name = NULL,
                       lfc_cutoff = 0,
                       shrink = FALSE,
                       annotation = "gtf",
                       anno_id = "gene_id",
                       anno_symbol = "gene_symbol")
        {

        if(is.null(result_name)){

                res <- results(dds,
                               contrast = c("Sample", contrast),
                               lfcThreshold = lfc_cutoff,
                               independentFiltering = FALSE)

                if(shrink){
                        res <- lfcShrink(dds, contrast = c("Sample", contrast), type="normal", res = res)
                }

        } else {

                res <- results(dds,
                               name = result_name,
                               lfcThreshold = lfc_cutoff,
                               independentFiltering = FALSE)

        }



        anno <- get(annotation)

        res[anno_symbol] <- mcols(anno)[anno_symbol][,1][match(rownames(res), mcols(anno)[anno_id][,1])]
        res$chr <- as.character(seqnames(anno))[match(rownames(res), mcols(anno)[anno_id][,1])]

        res$padj[is.na(res$padj)] <- 1
        res$log2FoldChange[is.na(res$log2FoldChange)] <- 0

        res <- res[order(res$pvalue),]

        return(res)

}

