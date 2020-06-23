#' plotGOGraph
#'
#' make a Gene Ontology enrichment table on DESeq2 results using topGO package
#' @export




plotGOGraph <- function(all_genes,
                        first_sig_nodes =  5,
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


        showSigOfNodes(tgd, score(resultTopGO), firstSigNodes = first_sig_nodes, useInfo = 'all')

}


