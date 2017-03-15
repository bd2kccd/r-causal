ftfc <- function(df, alpha = 0.05, useGap = TRUE, verbose = FALSE, java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)

    ftfc <- list()
    class(ftfc) <- "ftfc"

    ftfc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
    
    algorithm <- NULL
    if(userWishart){
        algorithm <- .jfield("edu.cmu.tetrad.search.FindTwoFactorClusters$Algorithm",,"GAP")
    }else{
        algorithm <- .jfield("edu.cmu.tetrad.search.FindTwoFactorClusters$Algorithm",,"SAG")
    }

    # Initiate ftfc
    ftfc_instance <- .jnew("edu/cmu/tetrad/search/FindTwoFactorClusters", tetradData, algorithm, alpha)
    .jcall(ftfc_instance, "V", "setVerbose", verbose)

    params <- c(params, alpha = alpha)
    params <- c(params, useGap = as.logical(useGap))
    params <- c(params, verbose = as.logical(verbose))

    ftfc$parameters <- params

    cat("Graph Parameters:\n")
    cat("alpha = ", alpha, "\n")
    cat("useGap = ", useGap,"\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(ftfc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    ftfc$nodes <- V

    # extract edges
    ftfc_edges <- extractTetradEdges(tetrad_graph)

    ftfc$edges <- ftfc_edges

    # convert output of ftfc into an R object (graphNEL)
    ftfc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    ftfc$graphNEL <- ftfc_graphNEL

    return(ftfc)
}