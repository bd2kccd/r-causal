pcstable <- function(df, continuous = TRUE, depth = 3, aggressivelyPreventCycles = FALSE, 
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

    pcstable <- list()
    class(pcstable) <- "pcstable"

    pcstable$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate PC-Stable
    pcstable_instance <- .jnew("edu/cmu/tetrad/search/Pc", indTest)
    .jcall(pcstable_instance, "V", "setDepth", as.integer(depth))
    .jcall(pcstable_instance, "V", "setAggressivelyPreventCycles", 
    	aggressivelyPreventCycles)
    .jcall(pcstable_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(pcstable_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = as.logical(continuous))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, aggressivelyPreventCycles = as.logical(aggressivelyPreventCycles))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    pcstable$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous, "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles, "\n")
    cat("significance = ", significance, "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(pcstable_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    pcstable$nodes <- V

    # extract edges
    pcstable_edges <- extractTetradEdges(tetrad_graph)

    pcstable$edges <- pcstable_edges

    # convert output of PC-Stable into an R object (graphNEL)
    pcstable_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    pcstable$graphNEL <- pcstable_graphNEL

    return(pcstable)
}