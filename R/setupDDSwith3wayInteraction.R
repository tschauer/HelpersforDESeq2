#' setupDDSwith3wayInteraction
#'
#' setup dds object with 3-way interaction term
#' @export



setupDDSwith3wayInteraction <- function(SampleTableName,
                                        CountTableName,
                                        SampleIdName,
                                        FactorAName,
                                        FactorBName,
                                        FactorCName,
                                        BatchName = NULL,
                                        n_samples_for_filtering = 3,
                                        min_number_of_reads = 1){

        SampleTable <- get(SampleTableName)

        my_counts_genes <- get(CountTableName)


        ########################################################

        stopifnot(identical(colnames(my_counts_genes), as.character(SampleTable[,SampleIdName])))


        filter <- apply(my_counts_genes, 1, function(x) length(x[x>min_number_of_reads]) >= n_samples_for_filtering)
        my_counts_filtered <- my_counts_genes[filter,]

        ########################################################

        my_colData <- DataFrame(FactorA = factor(SampleTable[,FactorAName]),
                                FactorB = factor(SampleTable[,FactorBName]),
                                FactorC = factor(SampleTable[,FactorCName]))
        rownames(my_colData) <- SampleTable[,SampleIdName]

        if(is.null(BatchName)){
                my_design = formula("~FactorA*FactorB*FactorC")
        } else {
                my_design = formula("~Batch+FactorA*FactorB*FactorC")
                my_colData$Batch <- factor(SampleTable[,BatchName])
        }

        ########################################################

        dds <- DESeqDataSetFromMatrix(countData = my_counts_filtered,
                                      colData = my_colData,
                                      design = my_design)


        ########################################################


        dds <- DESeq(dds)

        return(dds)
}


