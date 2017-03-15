fofc <- function(df, alpha = 0.05, useWishart = TRUE, useGap = TRUE, verbose = FALSE, java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)

    fofc <- list()
    class(fofc) <- "fofc"

    fofc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
    
    testType <- NULL
    if(userWishart){
        testType <- .jfield("edu.cmu.tetrad.search.TestType",,"TETRAD_WISHART")
    }elsea{
        testType <- .jfield("edu.cmu.tetrad.search.TestType",,"TETRAD_DELTA")
    }
    
    algorithm <- NULL
    if(userWishart){
        algorithm <- .jfield("edu.cmu.tetrad.search.FindOneFactorClusters$Algorithm",,"GAP")
    }elsea{
        algorithm <- .jfield("edu.cmu.tetrad.search.FindOneFactorClusters$Algorithm",,"SAG")
    }

    # Initiate fofc
    fofc_instance <- .jnew("edu/cmu/tetrad/search/FindOneFactorClusters", tetradData, testType, algorithm, alpha)
    .jcall(fofc_instance, "V", "setVerbose", verbose)

    params <- c(params, alpha = alpha)
    params <- c(params, useWishart = as.logical(useWishart))
    params <- c(params, useGap = as.logical(useGap))
    params <- c(params, verbose = as.logical(verbose))

    fofc$parameters <- params

    cat("Graph Parameters:\n")
    cat("alpha = ", alpha, "\n")
    cat("useWishart = ", useWishart,"\n")
    cat("useGap = ", useGap,"\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(fofc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    fofc$nodes <- V

    # extract edges
    fofc_edges <- extractTetradEdges(tetrad_graph)

    fofc$edges <- fofc_edges

    # convert output of fofc into an R object (graphNEL)
    fofc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    fofc$graphNEL <- fofc_graphNEL

    return(fofc)
}