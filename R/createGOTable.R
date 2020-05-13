#' createGOTable
#'
#' creates a Gene Ontology enrichment table on DESeq2 results using topGO package
#' @export




createGOTable <- function(all_genes,
                          shown_terms = 20,
                          min_signficant = 5,
                          select_ontology = "BP",
                          select_organism = "org.Mm.eg.db",
                          select_ID = "SYMBOL"){


        tgd <- new( "topGOdata",
                    ontology = select_ontology,
                    allGenes = all_genes,
                    nodeSize=5,
                    annot=annFUN.org,
                    mapping = select_organism,
                    ID =  select_ID)

        resultTopGO <- topGO::runTest(tgd, algorithm = "classic", statistic = "Fisher")


        bp <- topGO::GenTable( tgd,
                               Fisher.classic = resultTopGO,
                               orderBy = "Fisher.classic" , topNodes = 100)


        bp <- bp[bp$Significant >= min_signficant,]
        bp <- head(bp, shown_terms)
        bp$`Sign/Exp` <- bp$Significant / bp$Expected

        gene_list <- topGO::genesInTerm(object = tgd, whichGO = bp$GO.ID)
        gene_names <- unlist(lapply(gene_list, FUN = function(x){paste(x[x %in% (names(all_genes)[all_genes == 1])], collapse = "/")}))


        df <- merge(bp, data.frame(gene_names), by.x="GO.ID", by.y = "row.names")


        df$Fisher.classic <- gsub("< 1e-30","1e-30",df$Fisher.classic)
        df$Fisher.classic <- as.numeric(df$Fisher.classic)
        df <- df[order(df$Fisher.classic),]

        return(df)
}




########################################################  first letters up or down    ########################################################



firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
}


firstdown <- function(x) {
        substr(x, 1, 1) <- tolower(substr(x, 1, 1))
        x
}

