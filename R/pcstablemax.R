pcstablemax <- function(df, continuous = TRUE, depth = -1, maxPathLength = 3,
	useHeuristic = TRUE, significance = 0.05,
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

    pcmax <- list()
    class(pcmax) <- "pcmax"

    pcmax$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate PcMax
    pcmax_instance <- .jnew("edu/cmu/tetrad/search/PcStableMax", indTest)
    .jcall(pcmax_instance, "V", "setDepth", as.integer(depth))
    .jcall(pcmax_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(pcmax_instance, "V", "setUseHeuristic", useHeuristic)
    .jcall(pcmax_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(pcmax_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = continuous)
    params <- c(params, depth = as.integer(depth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, useHeuristic = as.logical(useHeuristic))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    pcmax$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous, "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength), "\n")
    cat("useHeuristic = ", useHeuristic, "\n")
    cat("significance = ", as.numeric(significance), "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(pcmax_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        pcmax$nodes <- NULL
        pcmax$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        pcmax$nodes <- V
        
        # extract edges
        pcmax_edges <- extractTetradEdges(tetrad_graph)
        
        pcmax$edges <- pcmax_edges
    }
    
    return(pcmax)
}
