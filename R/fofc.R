
fofc <- function(df, TestType = "TETRAD_WISHART", fofcAlgorithm = "GAP", 
    alpha = .01, java.parameters = NULL){
     
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

	## Converting to Tetrad dataset.
	df<-loadContinuousData(df)
    
	fofcAlgorithmPath <- "edu/cmu/tetrad/search/FindOneFactorClusters"
    TestTypePath <- "edu/cmu/tetrad/search/TestType"

    ## Instantiate fofc object.
    fofc_instance <- .jnew("edu/cmu/tetrad/search/FindOneFactorClusters", df, .jfield(TestTypePath,name=TestType), .jfield(paste(fofcAlgorithmPath, "$Algorithm", sep=""), name=fofcAlgorithm), as.double(alpha))

    ## Search
    fofc_graph <- .jcall(fofc_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

    ##List to contain results of fofc search.
    fofc <- list()

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
