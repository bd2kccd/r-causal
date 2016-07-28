cpc <- function(df, continuous = TRUE, depth = 3, aggressivelyPreventCycles = FALSE, 
	significance = 0.05, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
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

    cpc <- list()
    class(cpc) <- "cpc"

    cpc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate CPC
    cpc_instance <- .jnew("edu/cmu/tetrad/search/Cpc", indTest)
    .jcall(cpc_instance, "V", "setDepth", as.integer(depth))
    .jcall(cpc_instance, "V", "setAggressivelyPreventCycles", aggressivelyPreventCycles)
    .jcall(cpc_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(cpc_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, depth = as.integer(depth))
    params <- c(params, aggressivelyPreventCycles = as.logical(aggressivelyPreventCycles))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    cpc$parameters <- params

    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles, "\n")
    cat("significance = ", significance, "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(cpc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    cpc$nodes <- V

    # extract edges
    cpc_edges <- extractTetradEdges(tetrad_graph)

    cpc$edges <- cpc_edges

    # convert output of CPC into an R object (graphNEL)
    cpc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    cpc$graphNEL <- cpc_graphNEL

    return(cpc)
}