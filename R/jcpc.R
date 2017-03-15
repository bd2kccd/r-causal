jcpc <- function(df, softmaxAdjacencies = 8, maxIterations = 20, cpcDepth = -1
    aggressivelyPreventCycles = FALSE, significance = 0.05, verbose = FALSE, 
    java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)
    indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, significance)
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    jcpc <- list()
    class(jcpc) <- "jcpc"

    jcpc$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate jcpc
    jcpc_instance <- .jnew("edu/cmu/tetrad/search/Jcpc", indTest)
    .jcall(jcpc_instance, "V", "setSoftmaxAdjacencies", as.integer(softmaxAdjacencies))
    .jcall(jcpc_instance, "V", "setMaxIterations", as.integer(maxIterations))
    .jcall(jcpc_instance, "V", "setCpcDepth", as.integer(cpcDepth))
    .jcall(jcpc_instance, "V", "setAggressivelyPreventCycles", aggressivelyPreventCycles)
    .jcall(jcpc_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(jcpc_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = continuous)
    params <- c(params, softmaxAdjacencies = as.integer(softmaxAdjacencies))
    params <- c(params, maxIterations = as.integer(maxIterations))
    params <- c(params, cpcDepth = as.integer(cpcDepth))
    params <- c(params, aggressivelyPreventCycles = as.logical(aggressivelyPreventCycles))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    jcpc$parameters <- params

    cat("Graph Parameters:\n")
    cat("softmaxAdjacencies = ", as.integer(softmaxAdjacencies),"\n")
    cat("maxIterations = ", as.integer(maxIterations),"\n")
    cat("cpcDepth = ", as.integer(cpcDepth),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles, "\n")
    cat("significance = ", as.numeric(significance), "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(jcpc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    jcpc$nodes <- V

    # extract edges
    jcpc_edges <- extractTetradEdges(tetrad_graph)

    jcpc$edges <- jcpc_edges

    # convert output of jcpc into an R object (graphNEL)
    jcpc_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    jcpc$graphNEL <- jcpc_graphNEL

    return(jcpc)
}