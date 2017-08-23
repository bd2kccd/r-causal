

## Arguments: df=Dataset
## TestType = Either TETRAD_WISHART or TETRAD_DELTA
## fofcAlgorithm = Either GAP or SAG
## alpha = alpha value to be used in test.
## java.parameters = additional settings for java.
## Returns: latent variable clusters (as an edge list).
fofc <- function(df, TestType = "TETRAD_WISHART", fofcAlgorithm = "GAP", 
    alpha = .01, java.parameters = NULL){
     
    params <- list()
    params$java.parameters <- list()

    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    ## Converting to Tetrad dataset.
    df<-loadContinuousData(df)
    
	fofc <- list()
    class(fofc) <- "fofc"


    fofc$datasets <- deparse(substitute(data))


    params$TestType <- TestType
    params$fofcAlgorithm <- fofcAlgorithm
    params$alpha <-alpha

    fofc$parameters <- params

    fofcAlgorithmPath <- "edu/cmu/tetrad/search/FindOneFactorClusters"
    TestTypePath <- "edu/cmu/tetrad/search/TestType"

    ## Instantiate fofc object.
    fofc_instance <- .jnew("edu/cmu/tetrad/search/FindOneFactorClusters", df, .jfield(TestTypePath,name=TestType), .jfield(paste(fofcAlgorithmPath, "$Algorithm", sep=""), name=fofcAlgorithm), as.double(alpha))

    ## Search
    fofc_graph <- .jcall(fofc_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

    if(!is.null(e <- .jgetEx())){
        .jclear()
        fofc$nodes <- colnames(df)
        fofc$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(fofc_graph)

        ## Get nodes.
        fofc$nodes <- V
        
        ## extract edges
        fofc_edges <- extractTetradEdges(fofc_graph)
        
        fofc$edges <- fofc_edges
    }
    
    return(fofc)
}
