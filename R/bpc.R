bpc <- function(df, alpha = 0.05, useWishart = TRUE, verbose = FALSE, java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)

    bpc <- list()
    class(bpc) <- "bpc"

    bpc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
    
    testType <- NULL
    if(userWishart){
        testType <- .jfield("edu.cmu.tetrad.search.TestType",,"TETRAD_WISHART")
    }else{
        testType <- .jfield("edu.cmu.tetrad.search.TestType",,"TETRAD_DELTA")
    }

    purifyType <- .jfield("edu.cmu.tetrad.search.TestType",,"TETRAD_BASED")

    # Initiate bpc
    bpc_instance <- .jnew("edu/cmu/tetrad/search/BuildPureClusters", tetradData, alpha, testType, purifyType)
    .jcall(bpc_instance, "V", "setVerbose", verbose)

    params <- c(params, alpha = alpha)
    params <- c(params, useWishart = as.logical(useWishart))
    params <- c(params, verbose = as.logical(verbose))

    bpc$parameters <- params

    cat("Graph Parameters:\n")
    cat("alpha = ", alpha, "\n")
    cat("useWishart = ", useWishart,"\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(bpc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    bpc$nodes <- V

    # extract edges
    bpc_edges <- extractTetradEdges(tetrad_graph)

    bpc$edges <- bpc_edges

    # convert output of bpc into an R object (graphNEL)
    bpc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    bpc$graphNEL <- bpc_graphNEL

    return(bpc)
}