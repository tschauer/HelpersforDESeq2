#' getResults
#'
#' get results for comparisons
#' @export





getResults <- function(dds,
                       contrast,
                       lfc_cutoff = 0,
                       shrink = FALSE,
                       annotation = "gtf",
                       anno_id = "gene_id",
                       anno_symbol = "gene_symbol")
        {


        res <- results(dds,
                       contrast = contrast,
                       lfcThreshold = lfc_cutoff,
                       independentFiltering = FALSE)

        if(shrink){
                res <- lfcShrink(dds, contrast = contrast, type="normal", res = res)
        }

        anno <- get(annotation)

        res[anno_symbol] <- mcols(anno)[anno_symbol][,1][match(rownames(res), mcols(anno)[anno_id][,1])]
        res$chr <- as.character(seqnames(anno))[match(rownames(res), mcols(anno)[anno_id][,1])]

        res$padj[is.na(res$padj)] <- 1
        res$log2FoldChange[is.na(res$log2FoldChange)] <- 0

        res <- res[order(res$padj),]

        return(res)

}

