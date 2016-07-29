fci <- function(df, continuous = TRUE, depth = 3, significance = 0.05,
    noDSepSearch = FALSE, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    indTest = NULL
    if(continuous){
    	tetradData <- loadContinuousData(df)
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, significance)
    }else{
    	tetradData <- loadDiscreteData(df)
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestChiSquare", tetradData, 
    		significance)
    }
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    fci <- list()
    class(fci) <- "fci"

    fci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate FCI
    fci_instance <- .jnew("edu/cmu/tetrad/search/Fci", indTest)
    .jcall(fci_instance, "V", "setDepth", as.integer(depth))
    .jcall(fci_instance, "V", "setPossibleDsepSearchDone", !noDSepSearch)
    .jcall(fci_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fci_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = as.logical(continuous))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, noDSepSearch = as.logical(noDSepSearch))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fci$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("noDSepSearch = ", noDSepSearch,"\n")
    cat("significance = ", significance,"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fci_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    fci$nodes <- V

    # extract edges
    fci_edges <- extractTetradEdges(tetrad_graph)

    fci$edges <- fci_edges

    return(fci)
}