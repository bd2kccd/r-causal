pc <- function(df, continuous = TRUE, depth = 3, aggressivelyPreventCycles = FALSE, 
	falseDiscoveryRate = FALSE, significance = 0.05,
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
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

    pc <- list()
    class(pc) <- "pc"

    pc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate PC
    pc_instance <- .jnew("edu/cmu/tetrad/search/Pc", indTest)
    .jcall(pc_instance, "V", "setDepth", as.integer(depth))
    .jcall(pc_instance, "V", "setAggressivelyPreventCycles", aggressivelyPreventCycles)
    .jcall(pc_instance, "V", "setFdr", falseDiscoveryRate)
    .jcall(pc_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(pc_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = continuous)
    params <- c(params, depth = as.integer(depth))
    params <- c(params, aggressivelyPreventCycles = as.logical(aggressivelyPreventCycles))
    params <- c(params, falseDiscoveryRate = as.logical(falseDiscoveryRate))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    pc$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous, "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles, "\n")
    cat("falseDiscoveryRate = ", falseDiscoveryRate, "\n")
    cat("significance = ", as.integer(significance), "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(pc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    pc$nodes <- V

    # extract edges
    pc_edges <- extractTetradEdges(tetrad_graph)

    pc$edges <- pc_edges

    # convert output of PC into an R object (graphNEL)
    pc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    pc$graphNEL <- pc_graphNEL

    return(pc)
}